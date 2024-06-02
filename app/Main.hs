{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (msum, mzero)
import Data.ByteString.Char8 as C
import Happstack.Server (dir, nullConf, nullDir, ok, seeOther, simpleHTTP, toResponse, toResponseBS)
import Lucid
import Lucid.Htmx
import System.Random ( initStdGen, uniformR, RandomGen )
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Control.Monad.IO.Class (MonadIO(liftIO))

numbersTemplate :: Int -> Html ()
numbersTemplate n =
  p_ "List of numbers:"
    <> ul_
      ( mapM_ (li_ . toHtml . show) [1 .. n]
      )

data Expiration = Expired UTCTime | NotExpired UTCTime

connectedView :: Expiration -> Html ()
connectedView (Expired _) =
  p_ "Connected"
  <> p_ "Expired already"
connectedView (NotExpired _) =
  p_ "Connected"
  <> p_ "Expiring in xxx"
  <> button_ [
    hxPost_ "/disconnect"
  ] "Disconnect" 

disconnectedView :: Expiration -> Html ()
disconnectedView (Expired _) =
  p_ "Disconnected"
  <> p_ "Expired already"
disconnectedView (NotExpired _) =
  p_ "Disconnected"
  <> p_ "Expiring in xxx"
  <> button_ [
    hxPost_ "/connect"
  ] "Connect" 

generateDelay :: RandomGen g => g -> Int
generateDelay = fst . uniformR (3,10)

calcExpiration ::  Integer -> UTCTime -> UTCTime
calcExpiration = addUTCTime . fromInteger

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
          [ hxPost_ "/connect",
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
          t <- liftIO getCurrentTime
          ; g <- initStdGen
          ; let delay = toInteger $ generateDelay g
          ; let expiration = calcExpiration delay t
          ; dir "connect" $ ok $ toResponseBS (C.pack "text/html;charset=utf-8") $ renderBS $ connectedView (NotExpired expiration)
        },
        nullDir >> ok (toResponseBS (C.pack "text/html;charset=utf-8") $ renderBS $ doc ()),
        seeOther ("/" :: String) $ toResponse ("Goto to root" :: String)
      ]