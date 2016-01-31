module Calculator where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import String exposing (toInt)
import Time exposing (every, second)
import Json.Decode as Json


-- MODEL

type Ingredient = Coffee | Water | Ratio

type Unit = G | Oz

type alias Component =
  { title : Ingredient
  , value : Float
  , maxValue : Float
  , displayUnit : Unit
  , editing : Bool
  }

type alias Model =
  { coffee : Component
  , water : Component
  , ratio : Component
  , time : Int
  , ticking : Bool
  }

component : Ingredient -> Float -> Float -> Component
component title value maxValue =
  { title = title
  , value = value
  , maxValue = maxValue
  , displayUnit = G
  , editing = False
  }

initialModel : Model
initialModel =
  { coffee = component Coffee 20 100
  , water = component Water 320 1900
  , ratio = component Ratio 16 19
  , time = 0
  , ticking = False
  }


-- UPDATE

type Action =
  NoOp
  | ChangeCoffee (Unit -> Float -> Float)
  | ChangeWater (Unit -> Float -> Float)
  | ChangeRatio (Float -> Float)
  | ToggleUnit Ingredient
  | ToggleEdit Ingredient
  | UpdateVal Ingredient String
  | Tick
  | ToggleTicking
  | ResetTimer

update : Action -> Model -> Model
update action model =
  let
    coffee = .coffee model
    water = .water model
    ratio = .ratio model
  in
    case action of
      NoOp ->
        model

      ChangeCoffee op ->
        let
          newVal = op coffee.displayUnit coffee.value

          newCoffee =
            if newVal >= 0 && newVal <= coffee.maxValue then
              { coffee | value = newVal }
            else coffee

          newWater =
            { water | value = toFloat (round (newCoffee.value * ratio.value)) }
        in
          { model | coffee = newCoffee, water = newWater }

      ChangeWater op ->
        let
          newVal = op water.displayUnit water.value

          newWater =
            if newVal >= 0 && newVal <= water.maxValue then
              { water | value = newVal }
            else water

          newCoffee =
            { coffee | value = toDecimal (newWater.value / ratio.value) }
        in
          { model | coffee = newCoffee, water = newWater }

      ChangeRatio op ->
        let
          newVal = op ratio.value

          newRatio =
            if newVal >= 0 && newVal <= ratio.maxValue then
              { ratio | value = newVal }
            else ratio

          newWater =
            { water | value = toFloat (round (coffee.value * newRatio.value)) }
        in
          { model | water = newWater, ratio = newRatio }

      ToggleUnit ingredient ->
        let
          newCoffee =
            if ingredient == coffee.title then
              let toggledUnit = if coffee.displayUnit == G then Oz else G
              in { coffee | displayUnit = toggledUnit }
            else coffee

          newWater =
            if ingredient == water.title then
              let toggledUnit = if water.displayUnit == G then Oz else G
              in { water | displayUnit = toggledUnit }
            else water
        in
          { model | coffee = newCoffee, water = newWater }

      ToggleEdit ingredient ->
        let
          newCoffee =
            if ingredient == coffee.title then
              { coffee | editing = not coffee.editing }
            else coffee

          newWater =
            if ingredient == water.title then
              { water | editing = not water.editing }
            else water

          newRatio =
            if ingredient == ratio.title then
              { ratio | editing = not ratio.editing }
            else ratio

        in
          { model | coffee = newCoffee, water = newWater, ratio = newRatio }

      UpdateVal ingredient valString ->
        let
          val = parseFloat valString
        in
          if ingredient == Coffee then
            let
              valInGrams =
                if coffee.displayUnit == G then val
                else toDecimal (ozToG val)

              newCoffee =
                if valInGrams <= coffee.maxValue then
                  { coffee | value = valInGrams }
                else
                  { coffee | value = coffee.maxValue }

              newWater =
                if valInGrams <= coffee.maxValue then
                  { water | value = toFloat (round (valInGrams * ratio.value)) }
                else
                  { water | value = toFloat (round (coffee.maxValue * ratio.value)) }
            in
              { model | coffee = newCoffee, water = newWater }

          else if ingredient == Water then
            let
              valInGrams =
                if water.displayUnit == G then val
                else toFloat (round (ozToG val))

              newWater =
                if valInGrams / ratio.value <= coffee.maxValue then
                  { water | value = valInGrams }
                else
                  { water | value = toFloat (round (coffee.maxValue * ratio.value)) }

              newCoffee =
                if valInGrams / ratio.value <= coffee.maxValue then
                  { coffee | value = toDecimal (valInGrams / ratio.value) }
                else
                  { coffee | value = coffee.maxValue }
            in
              { model | coffee = newCoffee, water = newWater }

          else if ingredient == Ratio then
            let
              newRatio =
                if val <= ratio.maxValue then
                  { ratio | value = val }
                else
                  { ratio | value = ratio.maxValue }

              newWater =
                if val <= ratio.maxValue then
                  { water | value = toFloat (round (coffee.value * newRatio.value)) }
                else
                  { water | value = toFloat (round (coffee.value * ratio.maxValue)) }
            in
              { model | water = newWater, ratio = newRatio }

          else model

      Tick ->
        if model.ticking then { model | time = model.time + 1 } else model

      ToggleTicking ->
        { model | ticking = not model.ticking }

      ResetTimer ->
        { model | time = 0 }


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div [ id "container" ]
    [ div [ class "row"]
        [ componentPanel address model.coffee (ChangeCoffee incCoffee) (ChangeCoffee decCoffee)
        , componentPanel address model.water (ChangeWater incWater) (ChangeWater decWater)
        ]
    , div [ class "row" ]
        [ componentPanel address model.ratio (ChangeRatio incRatio) (ChangeRatio decRatio)
        , timer address model.time model.ticking
        ]
    ]

componentPanel : Address Action -> Component -> Action -> Action -> Html
componentPanel address component incAction decAction =
  let
    main =
      let
        displayValue =
          if component.displayUnit == G then component.value
          else toDecimal (gToOz component.value)
      in
        if component.editing then
          [ input
            [ type' "number"
            , pattern "[0-9]*"
            , attribute "inputmode" "numeric"
            , on "input" targetValue (Signal.message address << UpdateVal component.title)
            , onBlur address (ToggleEdit component.title)
            , onEnter address (ToggleEdit component.title)
            , value (toString displayValue)
            , autofocus True
            ] []
          ]
        else
          if component.title == Ratio then
            [ h1 [ onClick address (ToggleEdit component.title) ]
                [ text ("1:" ++ toString component.value) ]
            ]
          else
            [ h1 [ onClick address (ToggleEdit component.title) ]
                [ text (toString displayValue) ]
            , a [ class "unit", onClick address (ToggleUnit component.title) ]
                [ text (String.toLower (toString component.displayUnit)) ]
            ]
  in
    div [ classList [("column", True), ((toString component.title), True)] ]
      [ div [ class "flex-1" ]
          [ h2 [] [ text (toString component.title) ] ]
      , div [ class "flex-2" ]
          [ a [ class "up-button", onClick address incAction ] [] ]
      , div [ class "flex-3" ] main
      , div [ class "flex-2" ]
          [ a [ class "down-button", onClick address decAction ] [] ]
      , div [ class "flex-1" ] []
      ]

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

    resetButton =
      if not ticking && time /= 0 then
        a [ class "flex-1", onClick address ResetTimer ] [ text "reset" ]
      else a [] []
  in
    div [ classList [("column", True), ("timer", True)] ]
      [ div [ class "flex-1" ] [ h2 [] [ text "timer" ] ]
      , div [ class "flex-2" ] []
      , div [ class "flex-3" ] [ h1 [] [ text formattedTime ] ]
      , div [ class "flex-2" ]
          [ a [ class "flex-1", onClick address ToggleTicking ] [ text toggleText ]
          , resetButton
          ]
      , div [ class "flex-1" ] []
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

incCoffee : Unit -> Float -> Float
incCoffee unit n =
  if unit == G then (n * 10 + 1) / 10
  else toDecimal (n + (ozToG 1) / 10)

decCoffee : Unit -> Float -> Float
decCoffee unit n =
  if unit == G then (n * 10 - 1) / 10
  else toDecimal (n - (ozToG 1) / 10)

incWater : Unit -> Float -> Float
incWater unit n =
  if unit == G then n + 1
  else toFloat (round (n + (toDecimal (ozToG 1)) / 10))

decWater : Unit -> Float -> Float
decWater unit n =
  if unit == G then n - 1
  else toFloat (round (n - (toDecimal (ozToG 1)) / 10))

incRatio : Float -> Float
incRatio n =
  toDecimal ((n * 10 + 5) / 10)

decRatio : Float -> Float
decRatio n =
  toDecimal ((n * 10 - 5) / 10)

gToOz : Float -> Float
gToOz val =
  val * 35274 / 1000000

ozToG : Float -> Float
ozToG val =
  val * 283495 / 10000

toDecimal : Float -> Float
toDecimal val =
  let
    stringVal = toString val

    decimalIndex =
      case (List.head (String.indexes "." stringVal)) of
        Just value ->
          value
        Nothing ->
          String.length stringVal

    slicedFloat =
      case (String.toFloat (String.slice 0 (decimalIndex + 3) stringVal)) of
        Ok value ->
          value
        Err error ->
          0
  in
    (toFloat (round (slicedFloat * 10))) / 10

parseFloat : String -> Float
parseFloat string =
  case String.toFloat string of
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
