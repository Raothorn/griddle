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
update: GameState -> Direction -> GameState
update state direction =
    let
        letters = scanLetters state direction

        moveLetter letter gs = if moveValid gs letter.ix direction
                               then { letter | location = moveCoord direction letter.location }
                               else letter

        updateMove: Entity -> GameState -> GameState
        updateMove letter gs =
            let
                letter_ = moveLetter letter gs
                entities_ = Array.set letter.ix letter_ gs.entities
            in { gs | entities = entities_ }

    in
        List.foldl updateMove state letters

scanLetters: GameState -> Direction -> List Entity
scanLetters state direction =
    let
        rowIxs = List.range 0 (state.grid.rows - 1)
        colIxs = List.range 0 (state.grid.columns - 1)

        isLetter entity = case entity.eType  of
                              Letter _ -> True
                              _ -> False

        scanRow rowIx = Array.filter (\e -> isLetter e && e.location.row == rowIx) state.entities    |> Array.toList
        scanCol colIx = Array.filter (\e -> isLetter e && e.location.column == colIx) state.entities |> Array.toList

        scan: (Int -> List Entity) -> List Int -> List (List Entity)
        scan scanner range = List.map scanner range

        scanned = case direction of
                      Up -> scan scanRow rowIxs
                      Down -> scan scanRow (List.reverse rowIxs)
                      Left -> scan scanCol colIxs
                      Right -> scan scanCol (List.reverse colIxs)
    in
        List.concat scanned


moveValid: GameState -> Int -> Direction -> Bool
moveValid state entityIx direction =
    let
        entity = Array.filter (\e -> e.ix == entityIx) state.entities
               |> Array.toList
               |> List.head

        entityLoc = Maybe.map .location entity
        targetLoc = Maybe.map (moveCoord direction) entityLoc
        targetEntity = Maybe.andThen (entityAtLocation state.entities) targetLoc

        entityExists = case entity of
                           Just e -> not e.deleted
                           Nothing -> False

        entityBlocks eType = case eType of
                                 Rock -> True
                                 Letter _ -> True
                                 _ -> False

        targetBlocks = case targetEntity of
                           Just t -> entityBlocks t.eType
                           Nothing -> False

        inGrid = case targetLoc of
                     Just l -> inBounds state.grid l
                     Nothing -> False

    in
        entityExists && not targetBlocks && inGrid

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

testEntities = makeEntities [ (Letter 'A', Coordinate 0 0)
                            , (Letter 'B', Coordinate 0 1)
                            , (Rock, Coordinate 3 3)
                            ]
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
                            Rock -> '%'
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
