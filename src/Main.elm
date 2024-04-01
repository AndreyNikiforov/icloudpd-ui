module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    validatePassword model.password
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]

validatePassword : String -> Html msg
validatePassword pwd =
  let
    validation = combineValidators
      [
        upperValidator,
        lowerValidator,
        digitValidator,
        lengthValidator
      ] pwd
  in
    case validation of
      Nothing -> div [ style "color" "green" ] [ text "OK" ]
      Just v -> 
        let
          liError t = Html.li [] [text t]
        in 
          div [ style "color" "red" ] [
            text "Failed Validators:",
            ul [] (List.map liError v)
          ]

combineValidators : List (a -> Maybe b) -> a -> Maybe (List b)
combineValidators v s =
  combine (List.map (flip s) v)

combine: List (Maybe a) -> Maybe (List a)
combine l = 
  let
    filtered = List.filterMap identity l
  in 
    case filtered of
      [] -> Nothing
      _ -> Just filtered

upperValidator : String -> Maybe String
upperValidator = (validator hasUpper "Passwords must have Upper lcase letters")

lowerValidator : String -> Maybe String
lowerValidator =(validator hasLower "Passwords must have Lower case letter")

digitValidator : String -> Maybe String
digitValidator = (validator hasDigit "Passwords must have digits")

lengthValidator : String -> Maybe String
lengthValidator = (validator isLongEnough "Passwords must be longer than 8 characters")

flip : a -> (a -> b) -> b
flip s f = f s

validator : (a -> Bool) -> b -> a -> Maybe b
validator v e s =
  if v s then
    Maybe.Nothing
  else
    Maybe.Just e

hasUpper : String -> Bool
hasUpper = String.any Char.isUpper

hasLower : String -> Bool
hasLower = String.any Char.isLower

hasDigit : String -> Bool
hasDigit = String.any Char.isDigit

isLongEnough : String -> Bool
isLongEnough s = String.length s >= 8