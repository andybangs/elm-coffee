module View where

import Model exposing (Model, Component, Ingredient, Unit)
import Update exposing(..)
import Math exposing (..)
import Util exposing (gToOz, toDecimal)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)
import Signal exposing (Address)
import Json.Decode as Json

view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
    [ div [ class "row"]
        [ panel address model.coffee (ModCoffee incCoffee) (ModCoffee decCoffee)
        , panel address model.water (ModWater incWater) (ModWater decWater)
        ]
    , div [ class "row" ]
        [ panel address model.ratio (ModRatio incRatio) (ModRatio decRatio)
        , timer address model.time model.ticking
        ]
    ]

panel : Address Action -> Component -> Action -> Action -> Html
panel address component incAction decAction =
  let
    componentTitle =
      h2 [] [ text (toString component.title) ]

    componentMain =
      let
        displayValue =
          if component.displayUnit == Model.G then component.value
          else Util.toDecimal (Util.gToOz component.value)

        ingredientDisplay =
          h1 [ onClick address (ToggleEdit component.title) ]
            [ text (toString displayValue) ]

        ratioDisplay =
          h1 [ onClick address (ToggleEdit component.title) ]
            [ text ("1:" ++ toString component.value) ]
      in
        if component.editing then
          valInput address component.title displayValue
        else
          if component.title /= Model.Ratio then ingredientDisplay
          else ratioDisplay

    componentUnit =
      if component.title /= Model.Ratio then
        let unitText =
          if component.displayUnit == Model.G then "grams" else "ounces"
        in
          a [ class "unit", onClick address (ToggleUnit component.title) ]
            [ text unitText ]
      else
        a [] []

    opButtons =
      [ a [ classList [("button", True), ("button-left", True)]
          , onClick address decAction
          ]
          [ text "–" ]
      , a [ classList [("button", True), ("button-right", True)]
          , onClick address incAction
          ]
          [ text "+" ]
      ]

  in
    div [ classList [("column", True), ((toString component.title), True)] ]
      [ div [ class "flex-2" ] [ componentTitle ]
      , div [ class "flex-2" ] [ componentMain ]
      , div [ class "flex-1" ] [ componentUnit ]
      , div [ class "flex-1" ] []
      , div [ classList [("flex-1", True), ("button-cont", True)] ] opButtons
      , div [ class "flex-1" ] []
      ]

valInput : Address Action -> Ingredient -> Float -> Html
valInput address title displayValue =
  input [ type' "number"
    , pattern "[0-9]*"
    , attribute "inputmode" "numeric"
    , on "input" targetValue (Signal.message address << UpdateVal title)
    , onBlur address (ToggleEdit title)
    , onEnter address (ToggleEdit title)
    , value (toString displayValue)
    , autofocus True
    ]
    []

timer : Address Action -> Int -> Bool -> Html
timer address time ticking =
  let
    formattedTime =
      let
        minutes = time // 60
        seconds = time - minutes * 60
      in
        if seconds > 9 then (toString minutes) ++ ":" ++ (toString seconds)
        else (toString minutes) ++ ":0" ++ (toString seconds)

    toggleText =
      if ticking then "stop" else "start"

    timerButtons =
      let
        leftButton =
          a [ classList [("button", True), ("button-left", True)]
            , onClick address ToggleTicking
            ]
            [ text toggleText ]

        rightButton =
          a [ classList [("button", True), ("button-right", True)]
            , onClick address ResetTimer
            ]
            [ text "reset" ]
      in
        if not ticking && time /= 0 then [ leftButton, rightButton ]
        else [ leftButton ]
  in
    div [ classList [("column", True), ("timer", True)] ]
      [ div [ class "flex-2" ] [ h2 [] [ text "timer" ] ]
      , div [ class "flex-2" ] [ h1 [] [ text formattedTime ] ]
      , div [ class "flex-2" ] []
      , div [ classList [("flex-1", True), ("button-cont", True)] ] timerButtons
      , div [ class "flex-1" ] []
      ]

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
