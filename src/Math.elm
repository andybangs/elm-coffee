module Math where

import Model exposing (Unit)
import Util exposing (ozToG, toDecimal, roundFloat)

incCoffee : Unit -> Float -> Float
incCoffee unit n =
  if unit == Model.G then (n * 10 + 1) / 10
  else Util.toDecimal (n + (Util.ozToG 1) / 10)

decCoffee : Unit -> Float -> Float
decCoffee unit n =
  if unit == Model.G then (n * 10 - 1) / 10
  else Util.toDecimal (n - (Util.ozToG 1) / 10)

incWater : Unit -> Float -> Float
incWater unit n =
  if unit == Model.G then n + 1
  else Util.roundFloat (n + (Util.toDecimal (Util.ozToG 1)) / 10)

decWater : Unit -> Float -> Float
decWater unit n =
  if unit == Model.G then n - 1
  else Util.roundFloat (n - (Util.toDecimal (Util.ozToG 1)) / 10)

incRatio : Float -> Float
incRatio n =
  Util.toDecimal ((n * 10 + 5) / 10)

decRatio : Float -> Float
decRatio n =
  Util.toDecimal ((n * 10 - 5) / 10)
