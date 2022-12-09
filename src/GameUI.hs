{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module GameUI where

import Graphics.Vty hiding ((<|>))
import Control.Applicative ((<|>))

import Align(center, right, bottom)

import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson

import GHC.Generics

class (FromJSON a
      ,ToJSON   a) =>
       Perm a where

data Avatar = Avatar {
    _avX, _avY :: Int
  } deriving (Eq, Show, Generic)

instance Perm Avatar where

instance FromJSON Avatar where
  parseJSON = initially (Avatar 0 0)
instance ToJSON Avatar where

initial :: Perm a => a
initial = case Aeson.fromJSON Aeson.Null of
  Aeson.Error   e -> error $ "No default value: "
                       ++
                        show e
  Aeson.Success v -> v

initially init v = Aeson.genericParseJSON Aeson.defaultOptions v <|> pure init


uiMain = do
  
  Just world <- Aeson.decodeFileStrict "game.json"
  newWorld <- gameUI world
  Aeson.encodeFile "game.json" newWorld 
  return ()

gameUI world = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  dispsize <- displayBounds $ outputIface vty
  world <- eventLoop vty dispsize world
  shutdown vty
  return world

data World = World {
    message :: String
  , avatar  :: Avatar
  } deriving (Eq, Show, Generic)

instance ToJSON World where

instance FromJSON World where
  parseJSON = initially $ World "" initial

instance Perm World where

updateWorld (EvKey (KChar 'y') _)
  world = Just $ 
    world { message = "Hurrah!!!" }
updateWorld (EvKey (KChar 'n') _)
  world = Just $
    world { message = "Nah!!!" }
updateWorld (EvKey (KChar 'q') _) _ =
  Nothing
updateWorld (EvKey (KChar 'e') _)
  world = Just $
      world { avatar = moveAvatar 1 0
                  $ avatar world }
updateWorld _ world = Just world 

moveAvatar dx dy (Avatar x y) =
  Avatar (x+dx) (y+dy)

eventLoop vty dispSize world = do
  update vty $ display dispSize world
  evt      <- nextEvent vty
  newWorld <- pure $ updateWorld evt world
  case newWorld of
    Nothing          -> return world
    Just newerWorld  ->
      eventLoop vty dispSize
        newerWorld

class Display a where
  display :: DisplayRegion -> a -> Picture

instance Display World where
  display dispSize (World msg avatar) = 
    ( picForImage
    $ bottom dispSize
    $ string defAttr msg ) <>
    display dispSize avatar

instance Display Avatar where
  display dispSize (Avatar x y) =
    picForImage $
      translate x y $
        char defAttr '@'

