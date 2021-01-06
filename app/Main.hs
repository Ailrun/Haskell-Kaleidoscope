{-# LANGUAGE OverloadedStrings #-}
module Main where

import           CodeGen
import           Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Parser
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  ts <- parser <$> T.getContents
  T.putStrLn . T.concat . ("ready> " :) $ (<> "\nready> ") . T.pack . show <$> ts
  mt <- codeGen $ mapMaybe (either (const Nothing) Just) ts
  T.putStrLn mt
