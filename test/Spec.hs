import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Vision.Image.Type
import Vision.Image.RGBA.Type
import Vision.Primitive.Shape
import Data.Vector.Storable
import XMonad.Util.Icons
import System.IO.Unsafe

pixel :: Int -> Int -> Int -> RGBAPixel
pixel a b c = RGBAPixel (fromIntegral a) (fromIntegral b) (fromIntegral c) 255

main :: IO ()
main = hspec $
  describe "Icons.pixelToString" $ do

    it "should parse red" $ pixelToString (pixel 255 0 0) `shouldBe` "ff0000"
    it "should parse green" $ pixelToString (pixel 0 255 0) `shouldBe` "00ff00"
    it "should parse blue" $ pixelToString (pixel 0 0 255) `shouldBe` "0000ff"

    it "should parse light red" $ pixelToString (pixel 15 0 0) `shouldBe` "0f0000"
    it "should parse very light red" $ pixelToString (pixel 1 0 0) `shouldBe` "010000"
    it "should parse dark red" $ pixelToString (pixel 240 0 0) `shouldBe` "f00000"

    it "should parse light green" $ pixelToString (pixel 0 15 0) `shouldBe` "000f00"
    it "should parse very light green" $ pixelToString (pixel 0 1 0) `shouldBe` "000100"
    it "should parse dark green" $ pixelToString (pixel 0 240 0) `shouldBe` "00f000"

    it "should parse light blue" $ pixelToString (pixel 0 0 15) `shouldBe` "00000f"
    it "should parse very light blue" $ pixelToString (pixel 0 0 1) `shouldBe` "000001"
    it "should parse dark blue" $ pixelToString (pixel 0 0 240) `shouldBe` "0000f0"
