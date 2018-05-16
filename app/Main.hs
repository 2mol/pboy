module Main where

import Control.Monad (void)

import Brick (defaultMain)

import qualified UI

main :: IO ()
main = do
    initState <- UI.initState
    void $ defaultMain UI.app initState
