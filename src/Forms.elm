module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : Maybe Int
    , status : Maybe Bool
    }


init : Model
init =
    Model "" "" "" Nothing Nothing


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Age age ->
            { model | age = String.toInt age }

        Submit ->
            { model | status = Just (validateModel model) }


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewInput "age" "Age" (Maybe.withDefault "" (Maybe.map String.fromInt model.age)) Age
        , button [ onClick Submit ] [ text "Submit" ]
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


conditions : List (String -> Bool)
conditions =
    [ \s -> String.length s >= 8
    , String.any Char.isDigit
    , String.any Char.isUpper
    , String.any Char.isLower
    ]


validatePassword : String -> Bool
validatePassword password =
    List.all (\x -> x == True) (List.map (\fn -> fn password) conditions)


validateModel : Model -> Bool
validateModel model =
    model.password == model.passwordAgain && validatePassword model.password


viewValidation : Model -> Html Msg
viewValidation model =
    case model.status of
        Just True ->
            div [ style "color" "green" ] [ text "OK" ]

        Just False ->
            div [ style "color" "red" ] [ text "Invalid" ]

        Nothing ->
            text ""
