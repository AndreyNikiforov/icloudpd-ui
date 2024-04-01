module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (href)
import Dict exposing (Dict)
import Html.Attributes exposing (src)


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
  { dieFace : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate NewFace (Random.int 1 6)
      )

    NewFace newFace ->
      ( Model newFace
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
    images = Dict.fromList [
      (1, "https://www.shutterstock.com/image-illustration/face-dice-one-black-spot-over-1546355249"),
      (2, "https://www.shutterstock.com/image-illustration/face-dice-two-black-spots-over-1546355255"),
      (3, "https://www.shutterstock.com/image-illustration/face-dice-three-black-spots-over-1546355252"),
      (4, "https://www.shutterstock.com/image-illustration/face-dice-four-black-spots-over-1546355258"),
      (5, "https://www.shutterstock.com/image-illustration/face-dice-five-black-spots-over-1546355243"),
      (6, "https://www.shutterstock.com/image-illustration/face-dice-six-black-spots-over-1546355246")
      ]
    url = (Dict.get model.dieFace images)
  in
    case url of
      Nothing -> div [] [text "Error"]
      Just u -> 
        div []
          [ img [src u] []
          , button [ onClick Roll ] [ text "Roll" ]
          ]