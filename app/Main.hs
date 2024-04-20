{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad    (msum, mzero, forM_)
import Happstack.Server (nullConf, simpleHTTP, ok, toResponse)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A


numbersTemplate :: Int -> H.Html
numbersTemplate n = docTypeHtml $ do
        H.head $ do
            H.title "My First Page"
            H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        H.body $ do
            H.p "List of numbers:"
            H.ul $ forM_ [1..n] (li . toHtml)

main :: IO ()
main = simpleHTTP nullConf $ msum [
        mzero
        {- ,"Hello, World!" :: String -}
        , ok $ toResponse $ numbersTemplate 4
    ]