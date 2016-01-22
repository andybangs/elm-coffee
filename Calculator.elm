module Calculator where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Time exposing (every, second)


-- MODEL

type alias Model =
  { coffee : Int
  , water : Int
  , ratio : Int
  , time : Int
  , isTicking : Bool
  }

initialModel : Model
initialModel =
  { coffee = 20
  , water = 320
  , ratio = 16
  , time = 0
  , isTicking = False
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
  | Tick
  | Toggle
  | Reset

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

    Tick ->
      if model.isTicking then { model | time = model.time + 1 } else model

    Toggle ->
      { model | isTicking = not model.isTicking }

    Reset ->
      { model | time = 0 }


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
      let
        time =
          let
            minutes = model.time // 60
            seconds = model.time - minutes * 60
          in
            if seconds > 9 then
              (toString minutes) ++ ":" ++ (toString seconds)
            else
              (toString minutes) ++ ":0" ++ (toString seconds)

        toggleText : String
        toggleText =
          if model.isTicking then "stop" else "start"

        resetButton : Html
        resetButton =
          if not model.isTicking && model.time /= 0 then
            a [ class "start", onClick address Reset ] [ text "reset" ]
          else
            a [] []
      in
        div
          [ classList [("column", True), ("timer", True)] ]
          [ h2 [] [ text "timer" ]
          , h1 [] [ text time ]
          , a [ class "start", onClick address Toggle ] [ text toggleText ]
          , resetButton
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


-- INPUTS

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

ticker : Signal Action
ticker =
  Signal.map (always Tick) (Time.every Time.second)

model : Signal Model
model =
  Signal.foldp update initialModel (Signal.merge actions.signal ticker)

main : Signal Html
main =
  Signal.map (view actions.address) model
