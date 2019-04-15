{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module XMonad.Util.Icons (
    getIcons,
    xmobarIconWrapper,
    rgbaToXPM,
    pixelToString
) where

import XMonad
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.SetWMName

import qualified XMonad.Util.ExtensibleState as XS

import Data.Time.Clock
import Data.Time.Calendar

import qualified Data.Foldable as F

import Control.Monad (sequence)

import Foreign.C.String (castCCharToChar)

import Foreign.C.Types

import System.Directory

import XMonad.Actions.Minimize
import XMonad.Layout.Minimize

import qualified GI.Gtk as Gtk (init)
import qualified GI.Gtk
import qualified Data.Text as T
import           GI.Gtk.Objects.IconTheme
import           GI.GdkPixbuf.Objects.Pixbuf

import qualified XMonad.Layout.BoringWindows as BW

import Vision.Image.Transform (InterpolMethod(Bilinear), resize)

import qualified XMonad.StackSet as W

import XMonad.StackSet

import XMonad.Hooks.UrgencyHook

import System.Directory (getHomeDirectory)

import XMonad.Layout.NoBorders
import XMonad.Actions.Warp

import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as S
import Data.Bits
import Data.Vector.Storable (fromList)
import qualified Data.Vector.Storable as V
import Data.List
import qualified Data.List as L
import Data.Ord
import Data.Maybe

import Numeric (showHex)
import qualified GI.Gtk as Gtk (init)
import qualified GI.Gtk
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Vision.Image.Storage.DevIL
import Vision.Image.Storage.DevIL.ImageType
import Vision.Image.RGBA
import Vision.Image.Type
import Vision.Primitive

import System.Random
import System.IO.Unsafe

import Data.List.Split

import Control.Monad (mfilter)

import Data.Text (justifyRight, pack, unpack)
import           Data.Ini

themeLoadFlags :: [GI.Gtk.IconLookupFlags]
themeLoadFlags =
  [ GI.Gtk.IconLookupFlagsGenericFallback
  , GI.Gtk.IconLookupFlagsUseBuiltin
  ]

data Icon = Icon {
  width :: Int,
  height :: Int,
  rgba :: [Int]
  }

pixelToString :: RGBAPixel -> String
pixelToString pixel
  | alpha < 200 = "000000"
  | otherwise = do
        let red = unpack $ justifyRight 2 '0' $ pack $ showHex (rgbaRed pixel) ""
        let green = unpack $ justifyRight 2 '0' $ pack $ showHex (rgbaGreen pixel) ""
        let blue = unpack $ justifyRight 2 '0' $ pack $ showHex (rgbaBlue pixel) ""
        red ++ green ++ blue
   where alpha = rgbaAlpha pixel

colorsToString :: [String] -> String
colorsToString arr = intercalate "\n" $ map colorFun arr
  where
    colorFun :: String -> String
    colorFun c = "\"" ++ c ++ " c #" ++ c ++ "\","

colorLine :: [String] -> String
colorLine arr = "\"" ++ (intercalate "" arr) ++ "\""

createAsciiArt :: RGBA -> String
createAsciiArt icon = do
  let vector = manifestVector icon
  let (Z :. height :. width) = manifestSize icon
  let pixels = map pixelToString $ V.toList vector
  let a = chunksOf width pixels
  let lines = map colorLine a
  intercalate ",\n" lines

randomStr :: () -> String
randomStr _ = take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen

rgbaToXPM :: RGBA -> String
rgbaToXPM icon = do
  let vector = manifestVector icon
  let (Z :. height :. width) = manifestSize icon
  let pixels = map pixelToString $ V.toList vector
  let colors = nub pixels
  let header = "\"" ++ (show width) ++ " " ++ (show height) ++ " " ++ (show $ length colors) ++ " 6" ++ "\","
  let colorsPart = colorsToString colors
  let asciiArt = createAsciiArt icon
  "/* XPM */\nstatic char * icon_xpm[] = {\n" ++ header ++ "\n" ++ colorsPart ++ "\n" ++ asciiArt ++ "\n};"

iconToRGBA :: Icon -> RGBA
iconToRGBA x = Manifest size vector
  where
    size = ix2 (height x) (width x)
    vector = fromList $ map intToRGBA (rgba x)

resizeImage :: RGBA -> RGBA
resizeImage = resize Bilinear (ix2 16 16)

intToRGBA :: Int -> RGBAPixel
intToRGBA x = RGBAPixel
              ((.&.)(fromIntegral $ Data.Bits.shift x $ 8 * (-2)) 0xFF)
              ((.&.)(fromIntegral $ Data.Bits.shift x $ 8 * (-1)) 0xFF)
              ((.&.)(fromIntegral $ Data.Bits.shift x $ 8 * (-0)) 0xFF)
              ((.&.)(fromIntegral $ Data.Bits.shift x $ 8 * (-3)) 0xFF)

getIconFromArr :: [Foreign.C.Types.CLong] -> Icon
getIconFromArr arr = do
  let (width:height:integers) = map fromIntegral arr
  let rgba = take (width * height) integers
  Icon width height rgba

getIcon :: Window -> X (Maybe Icon)
getIcon win = withDisplay $ \dpy -> do
  a <- getAtom "_NET_WM_ICON"
  arr <- liftIO $ getWindowProperty32 dpy a win
  pure $ fmap getIconFromArr arr

getDistinctValues :: Icon -> Int
getDistinctValues icon = length $ nub $ rgba icon

getWindowClasses :: Window -> X (Maybe [String])
getWindowClasses win = withDisplay $ \dpy -> do
  chars <- io $ getWindowProperty8 dpy wM_CLASS win
  pure $ L.filter (not . null) . splitOn ['\x0'] . map castCCharToChar <$> chars

getThemeIcon :: String -> String -> IO (Maybe RGBA)
getThemeIcon pack name = do
  -- _ <- Gtk.init Nothing
  iconTheme <- iconThemeNew
  _ <- io $ iconThemeSetCustomTheme iconTheme $ Just $ T.pack pack
  let iconNameText = T.pack name
  iconExists <- iconThemeHasIcon iconTheme iconNameText
  case iconExists of 
    True -> do
      iconMaybe <- iconThemeLoadIcon iconTheme iconNameText 16 themeLoadFlags
      case iconMaybe of 
        Just icon -> do 
          bmp <- pixbufSaveToBufferv icon (T.pack "bmp") [] []
          let file = "/tmp/" ++ name ++ ".bmp"
          B.writeFile file bmp
          loaded <- load BMP file
          case loaded of
            Right (rgba :: RGBA) -> pure $ Just rgba
            Left _ -> pure Nothing
        Nothing -> pure Nothing
    False -> pure Nothing

getThemeIconForPacks :: [String] -> String -> IO (Maybe RGBA)
getThemeIconForPacks (pack:packs) name = do
  icon <- getThemeIcon pack name
  case icon of
    Just i -> pure $ Just i
    Nothing -> getThemeIconForPacks packs name
getThemeIconForPacks [] _ = pure Nothing

mapIOM :: X (Maybe a) -> (a -> b) -> X (Maybe b)
mapIOM iom f = do
  m <- iom
  pure $ f <$> m

getCurrentIconThemeName :: X (Maybe String)
getCurrentIconThemeName = do
  _ <- Gtk.init Nothing
  settings <- GI.Gtk.settingsGetDefault
  let iconName :: Maybe (X (Maybe T.Text)) = (GI.Gtk.getSettingsGtkIconThemeName <$> settings)
  case iconName of 
    Just i -> do
      iX <- i
      case iX of 
        Just iX1 -> pure $ Just $ T.unpack iX1
        Nothing -> pure Nothing
    Nothing -> pure Nothing

maybeFromEither :: Either b a -> Maybe a
maybeFromEither (Right x) = Just x
maybeFromEither (Left _) = Nothing

getCurrentIconThemeName2 :: X [String]
getCurrentIconThemeName2 = do
  ics <- XS.get :: X IconPackState
  let lc = lastCheck ics
  let p = packs ics
  currentTime <- io getCurrentTime
  let needsUpdate = diffUTCTime currentTime lc > 1
  case needsUpdate of
    True -> do
      homeDir <- io getHomeDirectory
      let configDir = homeDir ++ "/.config/gtk-3.0/settings.ini"
      exists <- io $ doesFileExist configDir
      case exists of 
        True -> do
          ini <- io $ readIniFile configDir

          case ini of
            Right ini -> do
              let iconTheme = maybeFromEither $ lookupValue "Settings" "gtk-icon-theme-name" ini
              let fallbackIconTheme = maybeFromEither $ lookupValue "Settings" "gtk-fallback-icon-theme" ini
              let theme = map unpack $ catMaybes [iconTheme, fallbackIconTheme]
              _ <- XS.put $ IconPackState currentTime theme
              pure theme
            Left _ -> do
              _ <- XS.put $ IconPackState currentTime p
              pure p

        False -> do
          _ <- XS.put $ IconPackState currentTime p
          pure p

    False -> pure p

data IconPackState = IconPackState {
  lastCheck :: UTCTime,
  packs :: [String]
}

instance ExtensionClass IconPackState where
  initialValue = IconPackState (UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0)) []

getIconString :: Window -> X (Maybe String)
getIconString win = do

  maybeClasses <- getWindowClasses win

  case maybeClasses of
    Just winClasses -> do

      -- iconThemeNameMaybe <- getCurrentIconThemeName
      -- let iconThemeName = maybeToList iconThemeNameMaybe

      iconThemes <- getCurrentIconThemeName2

      -- let iconName :: Maybe (IO (Maybe T.Text)) = (GI.Gtk.getSettingsGtkIconThemeName <$> settings)
      let winClass = intercalate "_" (winClasses ++ iconThemes)

      let fileName = "/tmp/xpms/" ++ winClass ++ ".xpm"
      exists <- io $ doesFileExist fileName

      case exists of
        False -> do

          let themeIcons = map (io . getThemeIconForPacks iconThemes) winClasses

          let b = mapIOM (getIcon win) iconToRGBA

          iconMaybe <- fmap F.asum . sequence $ themeIcons ++ [b]

          case iconMaybe of

            Just icon -> do
              let xpm = rgbaToXPM $ resizeImage icon
              _ <- io $ writeFile fileName xpm
              pure $ Just fileName
            Nothing -> pure Nothing
        True -> pure $ Just fileName
    Nothing -> pure Nothing

getIcons :: WorkspaceId -> X [String]
getIcons workspaceId = do

  let directory = "/tmp/xpms"

  _ <- io $ createDirectoryIfMissing False directory

  a <- gets windowset

  let findTag w = tag w == workspaceId

  let workspacesList = S.workspaces a

  let myWorkspace = fromJust $ find findTag workspacesList

  let windows = integrate' $ stack myWorkspace
  
  catMaybes <$> mapM getIconString windows

xmobarIconWrapper :: String -> String
xmobarIconWrapper = wrap "<icon=" "/>"
