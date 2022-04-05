module GameState exposing (..)

type alias GameState =
    { grid: Grid
    , entities: List Entity
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
    { id: Int
    , location: Coordinate
    , eType: EntityType
    , deleted: Bool
    }

type alias EntityManager =
    { entities: List Entity
    , candied: Maybe (List Int) -- A list of id's for entities which can move, if a candy has been touched
    }

moveEntities: EntityManager -> Direction -> EntityManager
moveEntities entityManager direction =
    let
        isLetter entity = case entity.eType of
                              Letter _ -> True
                              _ -> False

        movable = case entityManager.candied of
                      Just candied -> candied
                      Nothing -> List.filter isLetter entityManager.entities
                              |> List.map .id

        (newEntities, newCandied) = moveEntities_ entityManager.entities direction movable []
    in
        { entities = newEntities, candied = newCandied }

moveEntities_: List Entity -> Direction -> List Int -> List Int -> (List Entity, Maybe (List Int))
moveEntities_ entities direction movable candied =
    case movable of
        id::_ ->
            let
                matching = List.filter (\e -> e.id == id && e.deleted == False) entities
                entity = List.head matching
                entityLoc = Maybe.map .location entity
                targetLoc = Maybe.map (moveCoord direction) entityLoc
                targetEntity = Maybe.andThen (\l -> List.filter (\e -> targetLoc == Just e.location) entities |> List.head)

                -- otherEntities = List.filter (\e -> e.id /= id) entities
                -- newEntity = case targetEntity of
                --                 Just t -> 
                --                 Nothing ->

            in (entities, Nothing)
        [] -> (entities, Nothing)

moveCoord: Direction -> Coordinate -> Coordinate
moveCoord direction coord =
    case direction of
        Up -> { coord | row = coord.row - 1}
        Down -> { coord | row = coord.row + 1}
        Left -> { coord | row = coord.column - 1}
        Right -> { coord | row = coord.column + 1}

----------------
-- Primatives --
----------------
type Direction = Up | Down | Left | Right

type alias Coordinate =
    { row: Int
    ,column: Int
    }
