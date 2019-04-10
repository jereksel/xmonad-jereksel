module XMonad.Util.Icons (
    getIcons,
    xmobarIconWrapper,
    rgbaToXPM
) where

import XMonad
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.SetWMName

import Foreign.C.String (castCCharToChar)

import Foreign.C.Types

import System.Directory

import XMonad.Actions.Minimize
import XMonad.Layout.Minimize

import qualified XMonad.Layout.BoringWindows as BW

import Vision.Image.Transform (InterpolMethod(Bilinear), resize)

import qualified XMonad.StackSet as W

import XMonad.StackSet

import XMonad.Hooks.UrgencyHook

import XMonad.Layout.NoBorders
import XMonad.Actions.Warp

import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as S
import Data.Bits
import Data.Vector.Storable (fromList)
import qualified Data.Vector.Storable as V
import Data.List
import Data.Ord
import Data.Maybe

import Numeric (showHex)

import Vision.Image.RGBA
import Vision.Image.Type
import Vision.Primitive

import System.Random
import System.IO.Unsafe

import Data.List.Split

import Control.Monad (mfilter)

import Data.Text (justifyLeft, pack, unpack)

data Icon = Icon {
  width :: Int,
  height :: Int,
  rgba :: [Int]
  }

pixelToString :: RGBAPixel -> String
pixelToString pixel
  | alpha < 200 = "000000"
  | otherwise = do
        let red = unpack $ justifyLeft 2 '0' $ pack $ showHex (rgbaRed pixel) ""
        let green = unpack $ justifyLeft 2 '0' $ pack $ showHex (rgbaGreen pixel) ""
        let blue = unpack $ justifyLeft 2 '0' $ pack $ showHex (rgbaBlue pixel) ""
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
    size = ix2 (height x) (width x) :: Size
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
  let integers = map fromIntegral arr
  let width = integers!!0
  let height = integers!!1
  let rgba = take (width * height) $ drop 2 integers
  Icon width height rgba

getIcon :: Window -> X (Maybe Icon)
getIcon win = withDisplay $ \dpy -> do
  a <- getAtom "_NET_WM_ICON"
  arr <- liftIO $ getWindowProperty32 dpy a win
  pure $ fmap getIconFromArr arr

getDistinctValues :: Icon -> Int
getDistinctValues icon = length $ nub $ rgba icon

replace a b = map $ maybe b id . mfilter (/= a) . Just

pureX :: a -> X a
pureX = pure

getWindowClass :: Window -> X (Maybe String)
getWindowClass win = withDisplay $ \dpy -> do
  chars <- io $ getWindowProperty8 dpy wM_CLASS win
  pure $ replace '\x0' '_' . map castCCharToChar <$> chars

getIconString :: Window -> X (Maybe String)
getIconString win = do

  maybeClass <- getWindowClass win

  case maybeClass of
    Just winClass -> do

      let fileName = "/tmp/xpms/" ++ winClass ++ ".xpm"
      exists <- io $ doesFileExist fileName

      case exists of
        False -> do

          iconMaybe <- getIcon win

          case iconMaybe of

            Just icon -> do
              let xpm = rgbaToXPM $ resizeImage $ iconToRGBA icon
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
