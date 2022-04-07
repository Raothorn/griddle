module Main exposing (..)

import GameView exposing (..)
import GameState exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html.Attributes exposing (id, style, tabindex)
import Html.Events exposing (on)
import Time
import Element exposing (..)
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

type alias Model = GameState.GameManager

type Msg = HandleKeyboardEvent KeyboardEvent
         | Tick Time.Posix
         | NoOp

init: (Model, Cmd Msg)
init = (GameState.initialGame, Cmd.none)

update msg model =
    let
        newModel =
            case msg of
                HandleKeyboardEvent event -> updateKeyboard event model
                Tick time -> updateAnim model
                NoOp -> model
    in ( newModel, Cmd.none )

updateKeyboard: KeyboardEvent -> Model -> Model
updateKeyboard event model =
    let
        direction = case event.keyCode of
                        Keyboard.S -> Just GameState.Down
                        Keyboard.W -> Just GameState.Up
                        Keyboard.A -> Just GameState.Left
                        Keyboard.D -> Just GameState.Right
                        Keyboard.Down -> Just GameState.Down
                        Keyboard.Up -> Just GameState.Up
                        Keyboard.Left -> Just GameState.Left
                        Keyboard.Right -> Just GameState.Right
                        _    -> Nothing

        newStage = case model.stage of
                       Waiting info -> Maybe.map (updateInitialMove model.gamestate info.candiedLetters info.eatenCandies) direction
                                    |> Maybe.withDefault model.stage
                       _ -> model.stage
    in
        { model | stage = newStage }


updateAnim: Model -> Model
updateAnim model =
    let
        animDuration = 3
        newModel = case model.stage of
                       MoveProcessed moveInfo ->
                           let animStage = Animating { tickCount = 0
                                                     , duration = animDuration
                                                     , moveInfo = moveInfo }
                           in { model | stage = animStage }
                       Animating info ->
                           let
                               updatedState = info.moveInfo.updatedState
                           in
                               if info.tickCount < animDuration
                               then { model | stage = Animating { info | tickCount = info.tickCount + 1 }}
                               else { model | gamestate = updatedState, stage = FinishedAnimating }

                       FinishedAnimating -> { model | stage = updateResolveMove model.gamestate }
                       _ -> model
    in
        newModel

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)
    , Time.every (1000 / 60) Tick
    ]

view model =
    let
        edges = {top = True, bottom = False, left = True, right = False }
    in
        Element.layout [centerX, centerY] <|
            Element.column []
                [ Element.textColumn [spacing 10, padding 10 ]
                      [ paragraph [] [ text "WASD or arrow keys to move" ]
                      ]
                , el [Element.alignLeft] (gameview model)
                ]
