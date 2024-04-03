module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg as S exposing (circle, rect, svg)
import Svg.Attributes as SA exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFaces : ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ( 1, 1 )
    , generatePair
    )



-- UPDATE


type Msg
    = Roll
    | NewFaces ( Int, Int )


generatePair : Cmd Msg
generatePair =
    let
        faceGenerator =
            Random.int 1 6

        doubleFaceGenerator =
            Random.pair faceGenerator faceGenerator
    in
    Random.generate NewFaces doubleFaceGenerator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , generatePair
            )

        NewFaces faces ->
            ( { model | dieFaces = faces }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        circles : Int -> List ( Int, Int )
        circles dieFace =
            List.concat
                [ if 0 == modBy 2 dieFace then
                    []

                  else
                    [ ( 60, 60 ) ]
                , if dieFace > 1 then
                    [ ( 90, 30 ), ( 30, 90 ) ]

                  else
                    []
                , if dieFace > 3 then
                    [ ( 30, 30 ), ( 90, 90 ) ]

                  else
                    []
                , if dieFace > 5 then
                    [ ( 30, 60 ), ( 90, 60 ) ]

                  else
                    []
                ]

        buildCircle ( x, y ) =
            S.circle
                [ SA.cx (String.fromInt x)
                , SA.cy (String.fromInt y)
                , SA.r "10"
                , SA.stroke "white"
                , SA.fill "white"
                ]
                []

        viewCircle dieFace =
            S.svg
                [ SA.width "120"
                , SA.height "120"
                , SA.viewBox "0 0 120 120"
                ]
                (List.append
                    [ S.rect
                        [ SA.x "10"
                        , SA.y "10"
                        , SA.width "100"
                        , SA.height "100"
                        , SA.rx "15"
                        , SA.ry "15"
                        ]
                        []
                    ]
                    (List.map
                        buildCircle
                        (circles dieFace)
                    )
                )
    in
    div []
        [ viewCircle (Tuple.first model.dieFaces)
        , viewCircle (Tuple.second model.dieFaces)
        , button [ onClick Roll ] [ text "Roll" ]
        ]
