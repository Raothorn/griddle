module GameView exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Background as Background
import GameState exposing (..)

type TileContents = Text Char
                  | Image String
                  | NoContents

type alias Offset = (Float, Float)
type alias Tile = List (TileContents, Offset)

tileSize: Int
tileSize = 50

gameTiles: GameManager -> List (List Tile)
gameTiles manager =
    let
        gamestate = manager.gamestate
        getTile coord =
            let
                entities = entitiesAtLocation gamestate.entities coord

                entityTileContent entity =
                    if entity.deleted
                    then NoContents
                    else case entity.eType of
                             Letter ch -> Text ch
                             Rock -> Image "../resources/rock.png"
                             Candy -> Image "../resources/candy.png"
                             Belt Right -> Image "../resources/belt_right.png"
                             Belt Left -> Image "../resources/belt_left.png"
                             Belt Down -> Image "../resources/belt_down.png"
                             Belt Up -> Image "../resources/belt_up.png"
                             _ -> NoContents

                offset entity =
                  case manager.stage of
                      Animating info ->
                          let
                              offsetPerTick = (toFloat tileSize) / (toFloat info.duration)
                              pxOffset = offsetPerTick * (toFloat info.tickCount)

                              offsetInDir dir = case dir of
                                                    Up ->    (0, -pxOffset)
                                                    Down ->  (0, pxOffset)
                                                    Left ->  (-pxOffset, 0)
                                                    Right -> (pxOffset, 0)
                                                    _ -> (0, 0)

                              move = List.filter (\(ix, dir) -> ix == entity.ix) info.moveInfo.moves
                                   |> List.head
                          in
                              case move of
                                  Just (_, dir) -> offsetInDir dir
                                  _ -> (0, 0)
                      _ -> (0,0)

            in
                List.map (\e -> (entityTileContent e, offset e)) entities

        coordinates =
            let
                colList = List.range 0 (gamestate.grid.columns - 1)
                rowList = List.range 0 (gamestate.grid.rows - 1)
            in
                List.map (\r -> List.map (\c -> Coordinate r c) colList) rowList

    in
        List.map (\row -> List.map (\coord -> getTile coord) row) coordinates


gameview: GameManager -> Element msg
gameview manager =
    let
        state = manager.gamestate
        tiles = gameTiles manager
        getEdges coord =
                { top = coord.row == 0
                , bottom = coord.row == state.grid.rows - 1
                , left = coord.column == 0
                , right = coord.column == state.grid.rows - 1
                }

        gridRow: Int -> List Tile -> Element msg
        gridRow rowIx row =
            let
                coord colIx = Coordinate rowIx colIx
            in
                List.indexedMap
                    (\colIx tile -> gridTile (getEdges (coord colIx)) (coord colIx) state.grid tile)
                        row
                        |> Element.row []

        grid = List.indexedMap gridRow tiles
             |> Element.column []
    in
        el [centerX, centerY] grid


gridTile: {top: Bool, bottom: Bool, left: Bool, right: Bool} -> Coordinate
        -> Grid -> Tile -> Element msg
gridTile edges location grid tile =
    let
        border isEdge = if isEdge then 2 else 1

        wallBorder dir = if isEdgeWall grid location dir then 2 else 0

        borders =
            { top = border edges.top + wallBorder Up
            , bottom = border edges.bottom + wallBorder Down
            , left = border edges.left + wallBorder Left
            , right= border edges.right + wallBorder Right
            }

        imageContent = List.filter
                       (\(c, _) -> case c of
                                       Image _ -> True
                                       _ -> False)
                       tile
                     |> List.head

        letterContent = List.filter
                       (\(c, _) -> case c of
                                       Text _ -> True
                                       _ -> False)
                       tile
                      |> List.head

        background = case imageContent of
                         Just (Image src , _) -> [Background.image src]
                         _ -> []


        (letter, (offX, offY)) = case letterContent of
                                     Just (Text ch, os) -> (text (String.fromChar ch), os)
                                     _ -> (text "", (0, 0))

        inner = el [] letter
    in
        el
        ([ Border.widthEach borders
        , width (px 50), height (px 50)
        ] ++ background)
    <| el ([centerX, centerY, moveRight offX, moveDown offY]) inner
