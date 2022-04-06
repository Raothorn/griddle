module GameView exposing (..)

import Element exposing (..)
import Element.Border as Border
import GameState exposing (..)

gameview: GameState -> Element msg
gameview state =
    let
        charGrid = showGameState state


        getEdges coord =
            let
                top = coord.row == 0
                bottom = coord.row == state.grid.rows - 1
                left = coord.column == 0
                right = coord.column == state.grid.rows - 1
            in {top = top, bottom = bottom, left = left, right = right}

        rockUrl = "https://www.seekpng.com/png/full/396-3967087_rock-goron-mask-pixel-art.png"
        imgAttr = [width (px 40), height (px 40)]
        charToElement ch = case ch of
                               '%' -> image imgAttr
                                      {src = rockUrl, description = "rock"}
                               _ -> text (String.fromChar ch)

        gridRow: Int -> List Char -> Element msg
        gridRow rowIx row = List.indexedMap
                            (\colIx ch -> gridTile
                                          (getEdges (Coordinate rowIx colIx))
                                          (charToElement ch))
                            row
                          |> Element.row []

        grid = List.indexedMap gridRow charGrid
             |> Element.column []
    in
        el [centerX, centerY] <| grid


gridTile: {top: Bool, bottom: Bool, left: Bool, right: Bool}
        -> Element msg -> Element msg
gridTile edges contents =
    let
        border isEdge = if isEdge then 2 else 1
        borders =
            { top = border edges.top
            , bottom = border edges.bottom
            , left = border edges.left
            , right= border edges.right
            }
    in
        el
        [ Border.widthEach borders
        , width (px 50), height (px 50)
        ]
    <| el [centerX, centerY] contents
