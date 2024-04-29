{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (msum, mzero)
import Data.ByteString.Char8 as C
import Happstack.Server (dir, nullConf, nullDir, ok, seeOther, simpleHTTP, toResponse, toResponseBS)
import Lucid
import Lucid.Htmx
import System.Random

numbersTemplate :: Int -> Html ()
numbersTemplate n =
  p_ "List of numbers:"
    <> ul_
      ( mapM_ (li_ . toHtml . show) [1 .. n]
      )

generateDelay :: RandomGen g => g -> Int
generateDelay = fst . uniformR (3,10)

doc :: () -> Html ()
doc _ =
  doctypehtml_
    ( head_
        ( title_ "My First Page"
            <> meta_
              [ httpEquiv_ "Content-Type",
                content_ "text/html;charset=utf-8"
              ]
            <> script_ [src_ "https://unpkg.com/htmx.org@1.9.12"] ("" :: Html ())
        )
        <> body_
          [ hxGet_ "/body",
            hxTrigger_ "load delay:1s",
            hxSwap_ "innerHTML"
          ]
          (p_ "Loading..." :: Html ())
    )

main :: IO ()
main =
  simpleHTTP nullConf $
    msum
      [ mzero,
        do {
          g <- initStdGen
          ; dir "body" $ ok $ toResponseBS (C.pack "text/html;charset=utf-8") $ renderBS $ numbersTemplate $ generateDelay g -- mkStdGen 137
        },
        nullDir >> ok (toResponseBS (C.pack "text/html;charset=utf-8") $ renderBS $ doc ()),
        seeOther ("/" :: String) $ toResponse ("Goto to root" :: String)
      ]