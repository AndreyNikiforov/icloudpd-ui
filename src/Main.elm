module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Svg as S
import Svg.Attributes as SA exposing (fillOpacity)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Pause
    | Resume


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )

        Resume ->
            ( { model | paused = False }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Time.every 1000 Tick



-- VIEW


leftPadded : Int -> Int -> String
leftPadded k n =
    String.right k (String.fromInt (10 ^ k + n))


view : Model -> Html Msg
view model =
    let
        hour =
            Time.toHour model.zone model.time

        hourView =
            leftPadded 2 hour

        minuteView =
            leftPadded 2 minute

        minute =
            Time.toMinute model.zone model.time

        secondView =
            leftPadded 2 second

        second =
            Time.toSecond model.zone model.time

        handDegree t c =
            let
                totalF =
                    toFloat t

                currentF =
                    toFloat c

                ratio =
                    currentF / totalF
            in
            round (360 * ratio - 180)

        hourHandView h =
            S.rect
                [ SA.x "57"
                , SA.y "57"
                , SA.width "6"
                , SA.height "30"
                , SA.rx "2"
                , SA.ry "2"
                , SA.stroke "black"
                , SA.fill "black"
                , SA.fillOpacity "25%"
                , SA.transform ("rotate(" ++ String.fromInt (handDegree 12 h) ++ " 60 60)")
                ]
                []

        minuteHandView h =
            S.rect
                [ SA.x "58"
                , SA.y "58"
                , SA.width "4"
                , SA.height "50"
                , SA.rx "2"
                , SA.ry "2"
                , SA.stroke "black"
                , SA.fill "black"
                , SA.fillOpacity "25%"
                , SA.transform ("rotate(" ++ String.fromInt (handDegree 60 h) ++ " 60 60)")
                ]
                []

        secondHandView h =
            S.rect
                [ SA.x "59"
                , SA.y "59"
                , SA.width "2"
                , SA.height "60"
                , SA.rx "1"
                , SA.ry "1"
                , SA.stroke "red"
                , SA.fill "red"
                , SA.transform ("rotate(" ++ String.fromInt (handDegree 60 h) ++ " 60 60)")
                ]
                []

        markView h =
            S.rect
                [ SA.x "59"
                , SA.y "4"
                , SA.width "2"
                , SA.height "20"
                , SA.rx "1"
                , SA.ry "1"
                , SA.stroke "black"
                , SA.fill "black"
                , SA.strokeOpacity "90%"
                , SA.fillOpacity "90%"
                , SA.transform ("rotate(" ++ String.fromInt (handDegree 12 h) ++ " 60 60)")
                ]
                []

        markViews =
            List.map markView (List.range 0 12)
    in
    div []
        [ h1 [] [ text (hourView ++ ":" ++ minuteView ++ ":" ++ secondView) ]
        , if model.paused then
            button [ onClick Resume ] [ text "Resume" ]

          else
            button [ onClick Pause ] [ text "Pause" ]
        , S.svg
            [ SA.width "120"
            , SA.height "120"
            , SA.viewBox "0 0 120 120"
            ]
            (List.append
                markViews
                [ hourHandView hour
                , minuteHandView minute
                , secondHandView second
                ]
            )
        ]
