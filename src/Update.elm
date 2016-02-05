module Update where

import Model exposing (..)
import Util exposing (ozToG, toDecimal, roundFloat, parseFloat)

type Action =
  NoOp
  | ModCoffee (Unit -> Float -> Float)
  | ModWater (Unit -> Float -> Float)
  | ModRatio (Float -> Float)
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

      ModCoffee op ->
        let
          newVal = op coffee.displayUnit coffee.value

          newCoffee =
            if newVal >= 0 && newVal <= coffee.maxValue then
              { coffee | value = newVal }
            else coffee

          newWater =
            { water | value = Util.roundFloat (newCoffee.value * ratio.value) }
        in
          { model | coffee = newCoffee, water = newWater }

      ModWater op ->
        let
          newVal = op water.displayUnit water.value

          newWater =
            if newVal >= 0 && newVal <= water.maxValue then
              { water | value = newVal }
            else water

          newCoffee =
            { coffee | value = Util.toDecimal (newWater.value / ratio.value) }
        in
          { model | coffee = newCoffee, water = newWater }

      ModRatio op ->
        let
          newVal = op ratio.value

          newRatio =
            if newVal >= 0 && newVal <= ratio.maxValue then
              { ratio | value = newVal }
            else ratio

          newWater =
            { water | value = Util.roundFloat (coffee.value * newRatio.value) }
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
          val = Util.parseFloat valString
        in
          case ingredient of
            Coffee ->
              let
                valInGrams =
                  if coffee.displayUnit == G then val
                  else Util.toDecimal (Util.ozToG val)

                newCoffee =
                  if valInGrams <= coffee.maxValue then
                    { coffee | value = valInGrams }
                  else
                    { coffee | value = coffee.maxValue }

                newWater =
                  if valInGrams <= coffee.maxValue then
                    { water |
                      value = Util.roundFloat (valInGrams * ratio.value)
                    }
                  else
                    { water |
                      value = Util.roundFloat (coffee.maxValue * ratio.value)
                    }
              in
                { model | coffee = newCoffee, water = newWater }

            Water ->
              let
                valInGrams =
                  if water.displayUnit == G then val
                  else Util.roundFloat (Util.ozToG val)

                newWater =
                  if valInGrams / ratio.value <= coffee.maxValue then
                    { water | value = valInGrams }
                  else
                    { water |
                      value = Util.roundFloat (coffee.maxValue * ratio.value)
                    }

                newCoffee =
                  if valInGrams / ratio.value <= coffee.maxValue then
                    { coffee |
                      value = Util.toDecimal (valInGrams / ratio.value)
                    }
                  else
                    { coffee | value = coffee.maxValue }
              in
                { model | coffee = newCoffee, water = newWater }

            Ratio ->
              let
                newRatio =
                  if val <= ratio.maxValue then { ratio | value = val }
                  else { ratio | value = ratio.maxValue }

                newWater =
                  if val <= ratio.maxValue then
                    { water |
                      value = Util.roundFloat (coffee.value * newRatio.value)
                    }
                  else
                    { water |
                      value = Util.roundFloat (coffee.value * ratio.maxValue)
                    }
              in
                { model | water = newWater, ratio = newRatio }

      Tick ->
        if model.ticking then { model | time = model.time + 1 } else model

      ToggleTicking ->
        { model | ticking = not model.ticking }

      ResetTimer ->
        { model | time = 0 }
