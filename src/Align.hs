{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Module allowing to compose views from pictures.
module Align where

import Data.Char(isPrint, isSymbol, isAscii, isControl)
import Data.Semigroup
import qualified Data.Text as T
import Graphics.Vty
import Graphics.Vty.Image(DisplayRegion)
import Graphics.Vty.Input.Events ( Event(..), Key(..) )
import GHC.Generics(Generic)

-- Additional composition of `Picture`s
instance Semigroup Cursor where
  NoCursor <> a        = a
  a        <> NoCursor = a
  a        <> b        = b

instance Semigroup Picture where
  pic1 <> pic2 = Picture {
                     picCursor     = picCursor     pic1 <> picCursor pic2
                   , picLayers     = picLayers     pic1 <> picLayers pic2
                   , picBackground = picBackground pic2 -- just pick the last background
                  }

-- | For composable collection of views.
class Display a where
   display ::  DisplayRegion -- ^ Size of display region to use
           ->  a
           ->  Picture

-- | Center the Vty.Image by translation
center :: DisplayRegion -> Image -> Image
center (w,h) img = translate xoff yoff img
  where
    xoff = (w-imageWidth  img) `div` 2
    yoff = (h-imageHeight img) `div` 2

-- | Put the Vty.Image at the bottom by translation
bottom :: DisplayRegion -> Image -> Image
bottom (w, h) img = translate 0 yoff img
  where
    yoff = h - imageHeight img

-- | Fill the displayed string with spaces.
hfill :: DisplayRegion -> T.Text -> T.Text
hfill (w, h) str = str <> T.replicate (w - safeWctwidth str) " "

-- | Put the Vty.Image at the right edge by translation
right :: DisplayRegion -> Image -> Image
right (w, h) img = translate xoff 0 img
  where
    xoff = w - imageWidth img

stripUnprintableTrailer :: T.Text -> T.Text
stripUnprintableTrailer = T.dropWhileEnd (not . isPrint)

--instance {-# OVERLAPPABLE #-} Show a => Display a where
--  display disp = picForImage . center disp . string defAttr . show

instance Display String where
  display disp = picForImage . center disp . string defAttr

instance Display T.Text where
  display disp = picForImage . center disp . text' defAttr

-- * Narrow processing to a reasonable set of single-square characters (no CJK).
acceptableText :: T.Text -> Bool
acceptableText = T.all acceptableChar

acceptableChar :: Char -> Bool
acceptableChar c | isControl c = False
acceptableChar c | isAscii   c = True
-- FIXME: adding any Unicode symbol breaks character counting!
--acceptableChar c | isSymbol  c = True -- if we manage to filter characters that break length counting...
acceptableChar _               = False -- discard CJK etc.
