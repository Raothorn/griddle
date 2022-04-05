module Main exposing (..)

import GameView exposing (..)
import GameState

import Browser
import Browser.Events exposing (onKeyDown)
import Html.Attributes exposing (id, style, tabindex)
import Html.Events exposing (on)
import Element
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Keyboard

main: Program () Model Msg
main = Browser.element
       { init = always init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

type alias Model = GameState.GameState

type Msg = HandleKeyboardEvent KeyboardEvent
         | NoOp

init: (Model, Cmd Msg)
init = (GameState.initialGameState, Cmd.none)

update msg model =
    let
        newModel =
            case msg of
                HandleKeyboardEvent event ->
                    let
                        direction = case event.keyCode of
                                     Keyboard.S -> Just GameState.Down
                                     Keyboard.W -> Just GameState.Up
                                     Keyboard.A -> Just GameState.Left
                                     Keyboard.D -> Just GameState.Right
                                     _    -> Nothing

                    in case direction of
                           Just dir -> GameState.update model dir
                           Nothing -> model
                NoOp -> model
    in ( newModel, Cmd.none )


subscriptions: Model -> Sub Msg
subscriptions model =
    onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)

view model =
    let
        edges = {top = True, bottom = False, left = True, right = False }
    in
        Element.layout [] <| gameview model
