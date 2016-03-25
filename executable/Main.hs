module Main (main) where

import qualified Imgup

main :: IO ()
main = putStrLn =<< Imgup.uploadAndReturnUrl
