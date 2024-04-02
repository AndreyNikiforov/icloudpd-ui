module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random



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
    { dieFace1 : Int
    , dieFace2 : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | FirstFace Int
    | SecondFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Cmd.batch
                [ Random.generate FirstFace (Random.int 1 6)
                , Random.generate SecondFace (Random.int 1 6)
                ]
            )

        FirstFace face ->
            ( { model | dieFace1 = face }
            , Cmd.none
            )

        SecondFace face ->
            ( { model | dieFace2 = face }
            , Cmd.none
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ 
          div [] [ text (String.fromInt model.dieFace1) ]
        , div [] [ text (String.fromInt model.dieFace2) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
