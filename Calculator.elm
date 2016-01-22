module Calculator where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp


-- MODEL

type alias Model =
  { coffee : Int
  , water : Int
  , ratio : Int
  }

initialModel : Model
initialModel =
  { coffee = 20
  , water = 320
  , ratio = 16
  }


-- UPDATE

type alias Operation =
  Int -> Int

inc : Operation
inc n =
  n + 1

dec : Operation
dec n =
  n - 1

type Action =
  NoOp
  | Coffee Operation
  | Water Operation
  | Ratio Operation

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Coffee op ->
      let
        newCoffee = op model.coffee
      in
        { model |
          coffee = newCoffee
        , water = newCoffee * model.ratio
        }

    Water op ->
      let
        newWater = op model.water
      in
        { model |
          coffee = newWater // model.ratio
        , water = newWater
        }

    Ratio op ->
      let
        newRatio = op model.ratio
      in
        { model |
          water = model.coffee * newRatio
        , ratio = newRatio
        }


-- VIEW

view : Address Action -> Model -> Html
view address model =
  let
    component : String -> String -> Action -> Action -> Html
    component title display incAction decAction =
      div
        [ classList [("column", True), (title, True)] ]
        [ h2 [] [ text title ]
        , a [ class "up", onClick address incAction ] []
        , h1 [] [ text display ]
        , a [ class "down", onClick address decAction ] []
        ]

    timer : Html
    timer =
      div
        [ classList [("column", True), ("timer", True)] ]
        [ h2 [] [ text "timer" ]
        , h1 [] [ text "0:00" ]
        , a [ class "start"] [ text "start" ]
        ]
  in
    div
      [ id "container" ]
      [ div
          [ class "row"]
          [ component "coffee" (toString model.coffee) (Coffee inc) (Coffee dec)
          , component "water" (toString model.water) (Water inc) (Water dec)
          ]
      , div
          [ class "row" ]
          [ component "ratio" ("1:" ++ toString model.ratio) (Ratio inc) (Ratio dec)
          , timer
          ]
      ]


-- MAIN

main : Signal Html
main =
  StartApp.start
    { model = initialModel
    , view = view
    , update = update
    }
