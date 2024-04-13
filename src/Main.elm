module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import List exposing (foldl)
import Random exposing (initialSeed)
import Task
import Time
import Tuple



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Expiration =
    Time.Posix


type Connection
    = Connected Expiration
    | Disconnected Expiration


type alias Model =
    { time : Time.Posix
    , connection : Connection
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) (Disconnected (Time.millisToPosix 0))
    , generateDisconnect
    )



-- UPDATE


type Msg
    = Tick Time.Posix
      -- connection
    | Disconnect Expiration
    | Connect Expiration
    | StartDisconnect
    | StartConnect


timeToRandom : Random.Generator a -> Time.Posix -> a
timeToRandom g =
    Tuple.first << Random.step g << Random.initialSeed << Time.posixToMillis


disconnectGenerator : Random.Generator Int
disconnectGenerator =
    Random.int 5000 10000


connectGenerator : Random.Generator Int
connectGenerator =
    Random.int 10000 20000


generateExpiration : Random.Generator Int -> Time.Posix -> Time.Posix
generateExpiration g t =
    let
        offset =
            timeToRandom g t
    in
    (Time.millisToPosix << (+) offset << Time.posixToMillis) t


generateDisconnect : Cmd Msg
generateDisconnect =
    let
        cmd =
            Disconnect << generateExpiration disconnectGenerator
    in
    Task.perform cmd Time.now


generateConnect : Cmd Msg
generateConnect =
    let
        cmd =
            Connect << generateExpiration connectGenerator
    in
    Task.perform cmd Time.now


expireConnection : Connection -> Time.Posix -> Cmd Msg
expireConnection connection newTime =
    case connection of
        Disconnected exp ->
            if Time.posixToMillis exp < Time.posixToMillis newTime then
                generateConnect

            else
                Cmd.none

        Connected exp ->
            if Time.posixToMillis exp < Time.posixToMillis newTime then
                generateDisconnect

            else
                Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , expireConnection model.connection newTime
            )

        Disconnect exp ->
            ( { model | connection = Disconnected exp }, Cmd.none )

        Connect exp ->
            ( { model | connection = Connected exp }, Cmd.none )

        StartConnect ->
            ( model, generateConnect )

        StartDisconnect ->
            ( model, generateDisconnect )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


timeLeft : Time.Posix -> Time.Posix -> Maybe ( Int, String )
timeLeft s e =
    let
        em =
            Time.posixToMillis e

        sm =
            Time.posixToMillis s

        diffm =
            em - sm

        multipliers : List ( Int, String )
        multipliers =
            [ ( 1000 * 60 * 60 * 24 * 365, "year(s)" )
            , ( 1000 * 60 * 60 * 24 * 30, "month(s)" )
            , ( 1000 * 60 * 60 * 24, "day(s)" )
            , ( 1000 * 60 * 60, "hour(s)" )
            , ( 1000 * 60, "minute(s)" )
            , ( 1000, "second(s)" )
            ]

        div : Int -> ( Int, a ) -> ( Int, a )
        div d0 ( f0, s0 ) =
            ( d0 // f0, s0 )

        bucketed =
            List.map (div diffm) multipliers

        gtZero ( a, _ ) =
            a > 0

        maxBucket =
            bucketed
                |> List.filter gtZero
                |> List.head
    in
    maxBucket


view : Model -> Html Msg
view model =
    let
        expirationText : Maybe ( Int, String ) -> String
        expirationText tl =
            case tl of
                Just ( t, m ) ->
                    "Expires in " ++ String.fromInt t ++ " " ++ m

                Nothing ->
                    "Expired"

        -- expirationView : Expiration -> Model -> Html Msg
        -- expirationView expiration =
        --     p [] [ text expirationText (timeLeft model.time expiration)]
    in
    div []
        [ case model.connection of
            Connected expiration ->
                div []
                    [ p []
                        [ text ("Connected " ++ expirationText (timeLeft model.time expiration))
                        ]
                    , button
                        [ onClick StartDisconnect ]
                        [ text "Disconnect" ]
                    ]

            Disconnected expiration ->
                div []
                    [ p []
                        [ text ("Disconnected " ++ expirationText (timeLeft model.time expiration))
                        ]
                    , button
                        [ onClick StartConnect ]
                        [ text "Connect" ]
                    ]
        ]
