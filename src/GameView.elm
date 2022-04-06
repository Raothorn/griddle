module GameView exposing (..)

import Element exposing (..)
import Element.Border as Border
import GameState exposing (..)

type TileContents = Text Char
                  | Image String
                  | NoContents

type alias Tile = { contents: TileContents
                  , offset: (Float, Float)
                  }

tileSize: Int
tileSize = 50

gameTiles: GameState -> List (List Tile)
gameTiles gamestate =
    let
        getContents coord =
            let
                entity = entityAtLocation gamestate.entities coord
            in
                case entity of
                    Just e -> case e.eType of
                                  Letter ch -> Text ch
                                  Rock -> Image resources.rockUrl
                                  Belt _ -> Image resources.beltUrl
                                  _ -> NoContents
                    Nothing -> NoContents

        getOffset coord =
            case gamestate.stage of
                Animating info ->
                    let
                        entity = entityAtLocation gamestate.entities coord

                        offsetPerTick = (toFloat tileSize) / (toFloat info.duration)
                        pxOffset = offsetPerTick * (toFloat info.tickCount)

                        offsetInDir dir = case dir of
                                              Up ->    (0, -pxOffset)
                                              Down ->  (0, pxOffset)
                                              Left ->  (-pxOffset, 0)
                                              Right -> (pxOffset, 0)

                        offset = case entity of
                                     Just e ->
                                         if List.member e.ix info.moveInfo.movedLetters
                                         then offsetInDir info.moveInfo.moveDirection
                                         else (0, 0)
                                     Nothing -> (0, 0)
                    in
                        offset
                _ -> (0,0)

        coordinates =
            let
                colList = List.range 0 (gamestate.grid.columns - 1)
                rowList = List.range 0 (gamestate.grid.rows - 1)
            in
                List.map (\r -> List.map (\c -> Coordinate r c) colList) rowList

        getTile coord = Tile (getContents coord) (getOffset coord)
    in
        List.map (\row -> List.map (\coord -> getTile coord) row) coordinates


gameview: GameState -> Element msg
gameview state =
    let
        tiles = gameTiles state
        getEdges coord =
                { top = coord.row == 0
                , bottom = coord.row == state.grid.rows - 1
                , left = coord.column == 0
                , right = coord.column == state.grid.rows - 1
                }

        gridRow: Int -> List Tile -> Element msg
        gridRow rowIx row = List.indexedMap
                            (\colIx tile -> gridTile (getEdges (Coordinate rowIx colIx)) tile)
                            row
                          |> Element.row []

        grid = List.indexedMap gridRow tiles
             |> Element.column []
    in
        el [centerX, centerY] grid


gridTile: {top: Bool, bottom: Bool, left: Bool, right: Bool}
        -> Tile -> Element msg
gridTile edges tile =
    let
        border isEdge = if isEdge then 2 else 1
        borders =
            { top = border edges.top
            , bottom = border edges.bottom
            , left = border edges.left
            , right= border edges.right
            }

        inner = case tile.contents of
                    Text ch -> text (String.fromChar ch)
                    Image src -> image [width (px <| tileSize - 5), height (px <| tileSize - 5) ]
                                 { src = src, description = "Image Asset" }
                    _ -> Element.none

        innerOffset = case tile.offset of
                          (x, y) -> [moveDown y, moveRight x]
    in
        el
        [ Border.widthEach borders
        , width (px 50), height (px 50)
        ]
    <| el ([centerX, centerY] ++ innerOffset) inner

resources =
    { rockUrl = "../resources/rock.png"
    , beltUrl = "../resources/belt.png"
    }
