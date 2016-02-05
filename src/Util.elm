module Util where

import String exposing (length, indexes, slice, toFloat)

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
    roundFloat (slicedFloat * 10) / 10

roundFloat : Float -> Float
roundFloat val =
  Basics.toFloat (round val)

parseFloat : String -> Float
parseFloat string =
  case String.toFloat string of
    Ok value ->
      value
    Err error ->
      0
