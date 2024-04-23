{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, msum, mzero)
import Data.ByteString.Char8 as C
import qualified Data.Text as T
import Happstack.Server (nullConf, ok, simpleHTTP, toResponseBS)
import Lucid

numbersTemplate :: Int -> Html ()
numbersTemplate n =
  doctypehtml_
    ( head_
        ( title_ "My First Page"
            <> meta_
              [ httpEquiv_ "Content-Type",
                content_ "text/html;charset=utf-8"
              ]
        )
        <> body_
          ( p_ "List of numbers:"
              <> ul_
                ( mapM_ (li_ . toHtml . show) [1 .. n]
                )
          )
    )

main :: IO ()
main =
  simpleHTTP nullConf $
    msum
      [ mzero,
        {- ,"Hello, World!" :: String -}
        ok $ toResponseBS (C.pack "text/html;charset=utf-8") $ renderBS $ numbersTemplate 4
      ]