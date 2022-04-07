module GameState exposing (..)

import Array exposing (..)
import Set exposing (..)

type alias GameState =
    { grid: Grid
    , entities: Array Entity
    , stage: Stage
    }

type alias MoveInfo =
    { updatedState: GameState
    , moves: List (Int, Direction)
    }

type Stage = Waiting
           | InputReceived { moveDirection: Direction}
           | MoveProcessed MoveInfo
           | Animating { tickCount: Int, duration: Int, moveInfo: MoveInfo }
           | FinishedAnimating

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

entitiesAtLocation: Array Entity -> Coordinate -> List Entity
entitiesAtLocation entities location =
    Array.filter (\e -> e.location == location) entities
        |> Array.toList

----------------
-- Turn Logic --
----------------
updateInitialMove: GameState -> Direction -> Stage
updateInitialMove state direction =
    let
        letters = scanLetters state direction

        moveLetter letter gs = if moveValid gs letter.ix direction
                               then ({ letter | location = moveCoord direction letter.location }
                                    , True)
                               else (letter, False)

        updateMove: Entity -> (List Int, GameState) -> (List Int, GameState)
        updateMove letter (moved, gs) =
            let
                (letter_, didMove) = moveLetter letter gs
                entities_ = Array.set letter.ix letter_ gs.entities
                gs_= { gs | entities = entities_ }
                moved_ = if didMove then letter.ix::moved else moved
            in
                (moved_, gs_)

        (movedLetters, state_) = List.foldl updateMove ([], state) letters
        moves = List.map2 Tuple.pair movedLetters <|
                List.repeat (List.length movedLetters) direction

    in
        MoveProcessed { updatedState = state_
                      , moves = moves
                      }

updateResolveMove: GameState -> Stage
updateResolveMove state =
    let
        isBelt entity = case entity.eType of
                            Belt _ -> True
                            _ -> False

        beltDir entity = case entity.eType of
                             Belt dir -> dir
                             _ -> NoDirection

        beltUnderLetter: Entity -> Maybe (Entity, Entity)
        beltUnderLetter entity = case entity.eType of
                                     Letter _ ->
                                         Array.filter (\e -> isBelt e && e.location == entity.location) state.entities
                                             |> Array.map (\b -> (entity, b))
                                             |> Array.get 0
                                     _ -> Nothing

        letterBelts: List (Int, Direction)
        letterBelts = Array.map beltUnderLetter state.entities
                        |> Array.toList
                        |> List.filterMap identity
                        |> List.map (\(l, b) -> (l.ix, beltDir b))

    in
        Waiting

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

        scanned = case direction of
                      Up -> List.map scanRow rowIxs
                      Down -> List.map scanRow (List.reverse rowIxs)
                      Left -> List.map scanCol colIxs
                      Right -> List.map scanCol (List.reverse colIxs)
                      _ -> []
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
        targetEntity = Maybe.map (entitiesAtLocation state.entities) targetLoc
                     |> Maybe.andThen List.head

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

        canMove = (entityExists && not targetBlocks && inGrid)
    in
        entityExists && not targetBlocks && inGrid

----------------
-- Primatives --
----------------
type Direction = Up | Down | Left | Right | NoDirection

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
        NoDirection -> coord

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
                            , (Belt Right, Coordinate 2 1)
                            ]

initialGameState = GameState testGrid testEntities Waiting
