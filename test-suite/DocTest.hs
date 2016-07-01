{-# LANGUAGE NoImplicitPrelude #-}

module Main
    ( main
    ) where

import qualified System.IO as IO
import qualified Test.DocTest as DocTest


main :: IO.IO ()
main = do
    DocTest.doctest
        [ "library"
        ]
