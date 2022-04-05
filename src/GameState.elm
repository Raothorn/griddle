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
inBounds grid location = location.row > 0
                         && location.column > 0
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
type TurnState = InProgress TurnProgress
               | Finished

type alias TurnProgress =
    { hasMoved: Set Int
    , canMove:  Set Int
    , toMoveHint: Maybe Int
    }

moveLetters: Array Entity -> Direction -> Array Entity
moveLetters entities direction =
    let
        isLetter entity = case entity.eType of
                              Letter _ -> True
                              _ -> False
        canMove = Array.filter isLetter entities
                |> Array.map .ix
                |> Array.toList
                |> Set.fromList

        initProgress = TurnProgress Set.empty canMove Nothing
        (_, entities_) = hMoveLetters entities direction initProgress

    in
        entities_

hMoveLetters: Array Entity -> Direction -> TurnProgress -> (TurnProgress, Array Entity)
hMoveLetters entities direction progress =
    let
        remaining = Set.diff progress.canMove progress.hasMoved
    in
        if Set.isEmpty remaining then (progress, entities)
        else
            let
                toMoveIndex = case progress.toMoveHint of
                                    Just ix -> Just ix
                                    Nothing -> List.head (Set.toList remaining)

                toMoveEntity = Maybe.andThen (\ix -> Array.get ix entities) toMoveIndex

                targetLocation = toMoveEntity
                               |> Maybe.map .location
                               |> Maybe.map (moveCoord direction)

                targetEntity = Maybe.andThen (entityAtLocation entities) targetLocation
                targetIndex  = Maybe.map .ix targetEntity

                targetShouldMove = case targetIndex of
                                       Just ix -> Set.member ix progress.canMove &&
                                                  not (Set.member ix progress.hasMoved)
                                       Nothing -> False

                -- The hasMoved set if the entity did move
                hasMoved_ = if targetShouldMove then progress.hasMoved
                            else
                                case toMoveIndex of
                                    Just ix -> Set.insert ix progress.hasMoved
                                    Nothing -> progress.hasMoved

                toMoveHint_ = if targetShouldMove then targetIndex
                             else Nothing

                progress_ = { progress | hasMoved = hasMoved_, toMoveHint = toMoveHint_ }

                entity_ = if targetShouldMove then toMoveEntity
                          else
                              case toMoveEntity of
                                  Just entity -> Just { entity | location = Maybe.withDefault entity.location targetLocation }
                                  Nothing -> Nothing

                entities_ = case entity_ of
                                Just e_ -> Array.set e_.ix e_ entities
                                Nothing -> entities

            in hMoveLetters entities_ direction progress_


-- moveEntities: EntityManager -> Direction -> EntityManager
-- moveEntities entityManager direction =
--     let
--         isLetter entity = case entity.eType of
--                               Letter _ -> True
--                               _ -> False

--         movable = case entityManager.candied of
--                       Just candied -> candied
--                       Nothing -> List.filter isLetter entityManager.entities
--                               |> List.map .id

--         (newEntities, newCandied) = moveEntities_ entityManager.entities direction movable []
--     in
--         { entities = newEntities, candied = newCandied }

-- moveEntities_: List Entity -> Direction -> List Int -> List Int -> (List Entity, Maybe (List Int))
-- moveEntities_ entities direction movable candied =
--     case movable of
--         id::_ ->
--             let
--                 matching = List.filter (\e -> e.id == id && e.deleted == False) entities
--                 entity = List.head matching
--                 entityLoc = Maybe.map .location entity
--                 targetLoc = Maybe.map (moveCoord direction) entityLoc
--                 targetEntity = Maybe.andThen (\l -> List.filter (\e -> targetLoc == Just e.location) entities |> List.head)

--                 -- otherEntities = List.filter (\e -> e.id /= id) entities
--                 -- newEntity = case targetEntity of
--                 --                 Just t -> 
--                 --                 Nothing ->

--             in (entities, Nothing)
--         [] -> (entities, Nothing)


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

testGrid = Grid 3 3

testEntities = makeEntities [(Letter 'A', Coordinate 0 0)
                            ,(Letter 'B', Coordinate 0 1)]

initialGameState = GameState testGrid testEntities

showGameState: GameState -> String
showGameState state =
    let
        gridRows = Array.repeat state.grid.columns '.'
                 |> Array.repeat state.grid.rows

        addEntity: Entity -> Array (Array Char) -> Array (Array Char)
        addEntity entity rows =
            let
                eChar = case entity.eType of
                            Letter ch -> ch
                            _ -> '.'

                oldRow = Array.get entity.location.row rows
                newRow = case oldRow of
                             Just row -> Just (Array.set entity.location.column eChar row)
                             Nothing -> Nothing

            in case newRow of
                   Just row -> Array.set entity.location.row row rows
                   Nothing -> rows

        gridRows_= Array.foldr addEntity gridRows state.entities

        grid = Array.map (Array.toList) gridRows_ |> Array.toList
    in
        List.intersperse [' '] grid
        |> List.concat
        |> String.fromList

showEntities entities = showGameState (GameState testGrid entities)
