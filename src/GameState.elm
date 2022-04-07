module GameState exposing (..)

import Array exposing (..)
import Set exposing (..)

type alias GameManager =
    { gamestate: GameState
    , stage: Stage
    }

type alias GameState =
    { grid: Grid
    , entities: Array Entity
    }

type alias MoveInfo =
    { updatedState: GameState
    , moves: List Move
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
    , walls: List Wall
    }

type WallOrientation = VWall | HWall

type alias Wall =
    { orientation: WallOrientation
    , along: Int -- Wall lies along left or top (depending on orientation) side of 'along'
    , between: Int -- Wall spans from between - between + 1
    }

isEdgeWall: Grid -> Coordinate -> Direction -> Bool
isEdgeWall grid location edge =
    let
        wall = case edge of
                   Up -> Wall HWall location.row location.column
                   Down -> Wall HWall (location.row + 1) location.column
                   Left -> Wall VWall location.row location.column
                   Right -> Wall VWall location.row (location.column + 1)
                   _ -> Wall VWall (-1) (-1)
    in
        List.member wall grid.walls

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
type alias Move = (Int, Direction)

updateMoves: List Move -> GameState -> (List Move, GameState)
updateMoves moves state =
    let
        updateMove: Move -> (List Move, GameState) -> (List Move, GameState)
        updateMove move (moved, gs) =
            let
                (ix, dir) = move
                (valid, gs_) = moveValid gs ix dir

                moved_ = if valid then move::moved else moved
                newState = if valid then gs_ else gs
            in
                (moved_, newState)
    in
        List.foldl updateMove ([], state) moves

updateInitialMove: GameState -> Direction -> Stage
updateInitialMove state direction =
    let
        letters = scanLetters state direction
        moveCandidates = List.map2 Tuple.pair
                         (List.map .ix letters)
                         (List.repeat (List.length letters) direction)

        (moves, state_) = updateMoves moveCandidates state
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

        moveCandidates: List (Int, Direction)
        moveCandidates = Array.map beltUnderLetter state.entities
                       |> Array.toList
                       |> List.filterMap identity
                       |> List.map (\(l, b) -> (l.ix, beltDir b))

        (moves, state_) = updateMoves moveCandidates state
    in
        case moves of
            [] -> Waiting
            mvs -> MoveProcessed (MoveInfo state_ moves)

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


moveValid: GameState -> Int -> Direction -> (Bool, GameState)
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

        wallBlocks = case entityLoc of
                         Just l -> isEdgeWall state.grid l direction
                         Nothing -> False

        canMove = (entityExists && not targetBlocks && not wallBlocks && inGrid)

        entity_ = if canMove
                  then case entity of
                           Just e -> Maybe.map (\newLoc -> {e | location = newLoc } ) targetLoc
                           Nothing -> Nothing
                  else entity

        entities_ = case entity_ of
                        Just e_ -> Array.set entityIx e_ state.entities
                        Nothing -> state.entities

        state_ = { state | entities = entities_ }
    in
        (canMove, state_)

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

testGrid = Grid 5 5 [Wall VWall 1 4]

testEntities = makeEntities [ (Letter 'A', Coordinate 0 0)
                            , (Letter 'B', Coordinate 0 1)
                            , (Rock, Coordinate 3 3)
                            , (Belt Right, Coordinate 2 1)
                            ]

initialGame = GameManager (GameState testGrid testEntities) Waiting
