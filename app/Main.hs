{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (msum, mzero)
import Data.ByteString.Char8 as C
import Happstack.Server (nullConf, ok, simpleHTTP, toResponseBS, dir, seeOther)
import Lucid
import Lucid.Htmx

numbersTemplate :: Int -> Html ()
numbersTemplate n =
  p_ "List of numbers:"
    <> ul_
      ( mapM_ (li_ . toHtml . show) [1 .. n]
      )

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
        dir "body" $ ok $ toResponseBS (C.pack "text/html;charset=utf-8") $ renderBS $ numbersTemplate 4,
        ok $ toResponseBS (C.pack "text/html;charset=utf-8") $ renderBS $ doc ()
        -- ,seeOther "/" ("Redirect to Root" :: String)
      ]