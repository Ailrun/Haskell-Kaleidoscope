{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Either
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Parser
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  T.interact (T.concat . mainLoop)
