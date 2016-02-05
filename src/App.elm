module App where

import Model exposing (Model, initialModel)
import Update exposing (Action, update)
import View exposing (view)

import Html exposing (Html)
import Signal exposing (Address)
import Time exposing (every, second)

actions : Signal.Mailbox Action
actions =
  Signal.mailbox Update.NoOp

ticker : Signal Action
ticker =
  Signal.map (always Update.Tick) (Time.every Time.second)

input : Signal Action
input =
  Signal.merge actions.signal ticker

model : Signal Model
model =
  Signal.foldp update initialModel input

main : Signal Html
main =
  Signal.map (view actions.address) model
