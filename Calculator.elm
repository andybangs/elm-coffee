module Calculator where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Time exposing (every, second)
import Json.Decode as Json
import String exposing (toInt)


-- MODEL

type Editing = None | CoffeeVal | WaterVal | RatioVal

type alias Model =
  { coffee : Int
  , water : Int
  , ratio : Int
  , editing : Editing
  , time : Int
  , ticking : Bool
  }

initialModel : Model
initialModel =
  { coffee = 20
  , water = 320
  , ratio = 16
  , editing = None
  , time = 0
  , ticking = False
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
  | ToggleEdit Editing
  | UpdateVal String String
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

    ToggleEdit editVal ->
      { model | editing = editVal }

    UpdateVal title valString ->
      let
        val = parseInt valString
      in
        if title == "coffee" then
          { model | coffee = val, water = val * model.ratio }
        else if title == "water" then
          { model | coffee = val // model.ratio, water = val }
        else if title == "ratio" then
          { model | water = model.coffee * val, ratio = val }
        else model

    Tick ->
      if model.ticking then { model | time = model.time + 1 } else model

    Toggle ->
      { model | ticking = not model.ticking }

    Reset ->
      { model | time = 0 }


-- VIEW

view : Address Action -> Model -> Html
view address model =
  let
    component : String -> Int -> Editing -> Action -> Action -> Html
    component title val editVal incAction decAction =
      let
        main =
          if model.editing == editVal then
            input
              [ type' "number"
              , pattern "[0-9]*"
              , attribute "inputmode" "numeric"
              , on "input" targetValue (Signal.message address << UpdateVal title)
              , onBlur address (ToggleEdit None)
              , onEnter address (ToggleEdit None)
              , value (toString val)
              , autofocus True
              ]
              [ ]
          else
            h1 [ onClick address (ToggleEdit editVal) ] [ text (toString val) ]
      in
        div
          [ classList [("column", True), (title, True)] ]
          [ h2 [ class "title" ] [ text title ]
          , a [ class "up", onClick address incAction ] []
          , div [ class "main" ] [ main ]
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
          if model.ticking then "stop" else "start"

        resetButton : Html
        resetButton =
          if not model.ticking && model.time /= 0 then
            a [ class "button", onClick address Reset ] [ text "reset" ]
          else
            a [] []
      in
        div
          [ classList [("column", True), ("timer", True)] ]
          [ h2 [ class "timer-title" ] [ text "timer" ]
          , h1 [ class "timer-main" ] [ text time ]
          , div [ class "start" ]
              [ a [ class "button", onClick address Toggle ] [ text toggleText ]
              , resetButton
              ]
          ]
  in
    div
      [ id "container" ]
      [ div
          [ id "topRow", class "row"]
          [ component "coffee" model.coffee CoffeeVal (Coffee inc) (Coffee dec)
          , component "water" model.water WaterVal (Water inc) (Water dec)
          ]
      , div
          [ id "bottomRow", class "row" ]
          [ component "ratio" model.ratio RatioVal (Ratio inc) (Ratio dec)
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

-- HELPERS

parseInt : String -> Int
parseInt string =
  case String.toInt string of
    Ok value ->
      value
    Err error ->
      0

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
