module GameState exposing (..)

import Array exposing (..)
import Set exposing (..)

type alias GameState =
    { grid: Grid
    , entities: Array Entity
    }

----------
-- Grid --
----------
type alias Grid =
    { rows: Int
    , columns: Int
    }

inBounds: Grid -> Coordinate -> Bool
inBounds grid location = location.row >= 0
                         && location.column >= 0
                         && location.row < grid.rows
                         && location.column < grid.columns

--------------
-- Entities --
--------------
type EntityType = Letter Char
                | Belt Direction
                | Rock
                | Candy

type alias Entity =
    { ix: Int -- For convenience, we store the array index in the entity itself
    , eType: EntityType
    , location: Coordinate
    , deleted: Bool
    }

entityAtLocation: Array Entity -> Coordinate -> Maybe Entity
entityAtLocation entities location =
    Array.filter (\e -> e.location == location) entities
        |> Array.toList
        |> List.head

----------------
-- Turn Logic --
----------------
type alias Move =
    { entityIx: Int
    , direction: Direction
    }

update: GameState -> Direction -> GameState
update state direction =
    let
        isLetter entity = case entity.eType  of
                              Letter _ -> True
                              _ -> False
        letters = Array.filter isLetter state.entities
        initMoveStack = Array.map (\e -> Move e.ix direction) letters
                      |> Array.toList

        moveList = getMoveList state initMoveStack
        -- dummy = Debug.log "move list" moveList
    in
        state

getMoveList: GameState -> List Move -> List Move
getMoveList state moveStack =
    let
        executedMoves = []

        next prevStack executed =
            case prevStack of
                [] -> executed
                (nextMove::remaining) ->
                    let
                        _ = Debug.log "movestack" prevStack
                        _ = Debug.log "executed" executed
                        (isValid, movableTarget) = moveValid state nextMove

                        prependedMove = case movableTarget of
                                            Just t -> Just (Move t.ix nextMove.direction)
                                            Nothing -> Nothing
                        _ = Debug.log "prependedMove" prependedMove

                        moveStack_ = case prependedMove of
                                         Just pm ->
                                             let
                                                 deleted = List.filter (\m -> m.entityIx /= pm.entityIx) prevStack
                                                 _ = Debug.log "deleted " deleted
                                             in pm::deleted
                                         Nothing -> remaining
                        _ = Debug.log "movestack'" prevStack
                        _ = Debug.log "movestack'" remaining

                        executedMove = case prependedMove of
                                           Just pm -> []
                                           Nothing ->
                                               if isValid then [nextMove] else []

                        executed_ = executed ++ executedMove

                    in
                        if List.member nextMove executed
                        then next remaining executed
                        else next moveStack_ executed_
    in next moveStack []

moveValid: GameState -> Move -> (Bool, Maybe Entity)
moveValid state move =
    let
        entity = Array.filter (\e -> e.ix == move.entityIx) state.entities
               |> Array.toList
               |> List.head


        entityLoc = Maybe.map .location entity
        targetLoc = Maybe.map (moveCoord move.direction) entityLoc
        targetEntity = Maybe.andThen (entityAtLocation state.entities) targetLoc

        entityExists = case entity of
                           Just e -> not e.deleted
                           Nothing -> False

        entityBlocks eType = case eType of
                                 Rock -> True
                                 _ -> False

        targetBlocks = case targetEntity of
                           Just t -> entityBlocks t.eType
                           Nothing -> False

        inGrid = case targetLoc of
                     Just l -> inBounds state.grid l
                     Nothing -> False

        movableTargetEntity = case targetEntity of
                                   Just e -> case e.eType of
                                                 Letter _ -> Just e
                                                 _ -> Nothing
                                   Nothing -> Nothing

        canMove = entityExists && not targetBlocks && inGrid
    in
        (canMove, movableTargetEntity)

----------------
-- Primatives --
----------------
type Direction = Up | Down | Left | Right

type alias Coordinate =
    { row: Int
    , column: Int
    }

moveCoord: Direction -> Coordinate -> Coordinate
moveCoord direction coord =
    case direction of
        Up ->    { coord | row = coord.row - 1}
        Down ->  { coord | row = coord.row + 1}
        Left ->  { coord | column = coord.column - 1}
        Right -> { coord | column = coord.column + 1}

-------------
-- Testing --
-------------
makeEntities: List (EntityType, Coordinate) -> Array Entity
makeEntities entities = List.indexedMap (\ix (t, l) -> Entity ix t l False) entities
                      |> Array.fromList

testGrid = Grid 5 5

testEntities = makeEntities [(Letter 'A', Coordinate 0 0)
                            ,(Letter 'B', Coordinate 0 1)]
initialGameState = GameState testGrid testEntities

showGameState: GameState -> List (List Char)
showGameState state =
    let
        gridRows = Array.repeat state.grid.columns ' '
                 |> Array.repeat state.grid.rows

        addEntity: Entity -> Array (Array Char) -> Array (Array Char)
        addEntity entity rows =
            let
                eChar = case entity.eType of
                            Letter ch -> ch
                            _ -> ' '

                oldRow = Array.get entity.location.row rows
                newRow = case oldRow of
                             Just row -> Just (Array.set entity.location.column eChar row)
                             Nothing -> Nothing

            in case newRow of
                   Just row -> Array.set entity.location.row row rows
                   Nothing -> rows

        gridRows_= Array.foldr addEntity gridRows state.entities

        grid = Array.map (Array.toList) gridRows_ |> Array.toList
    in grid

showEntities entities = showGameState (GameState testGrid entities)
