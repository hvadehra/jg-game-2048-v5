module Main exposing (main)

import Html exposing (..)
import Html.Attributes as HA exposing (style)
import List.Extra as LE
import Random exposing (Generator)
import Random.List


randomTilesCount =
    4


main =
    let
        initialTiles : List Tile
        initialTiles =
            Random.step (randomTiles allPositions) (Random.initialSeed 2)
                |> Tuple.first
                --|> Debug.log ""
                |> always initialTiles2

        initialTiles2 =
            [ Tile ( 3, 1 ) 2
            , Tile ( 2, 1 ) 2
            , Tile ( 2, 2 ) 2
            ]

        --_ =
        --    List.length initialTiles |> Debug.log "len"
    in
    view
        { tiles =
            initialTiles

        --|> slideTilesInDirection Down
        --|> slideTilesInDirection Left
        --|> slideTilesInDirection Right
        --|> slideTilesInDirection Left
        --|> slideTilesInDirection Up
        --|> slideTilesInDirection Down
        --|> slideTilesInDirection Down
        }


type alias GridPos =
    ( Int, Int )


type alias Tile =
    { pos : GridPos
    , value : TileValue
    }


type alias TileValue =
    Int


randomTiles : List GridPos -> Generator (List Tile)
randomTiles gridPositions =
    randomAvailablePositionsUpto randomTilesCount gridPositions
        |> Random.andThen randomTilesFromGridPos


randomAvailablePositionsUpto : Int -> List GridPos -> Generator (List GridPos)
randomAvailablePositionsUpto maxPositions availablePositions =
    Random.List.shuffle availablePositions
        |> Random.map (List.take maxPositions)


randomTilesFromGridPos : List GridPos -> Generator (List Tile)
randomTilesFromGridPos gridPositions =
    let
        len =
            List.length gridPositions
    in
    Random.list len randomInitialValue
        |> Random.map
            (\vals ->
                List.map2 Tuple.pair gridPositions vals
                    |> List.map (\( pos, value ) -> initTile pos value)
            )


randomInitialValue : Generator TileValue
randomInitialValue =
    Random.uniform 2 [ 4 ]


initTile : GridPos -> TileValue -> Tile
initTile pos value =
    Tile pos value


allPositions =
    List.range 0 3
        |> List.concatMap (\x -> List.range 0 3 |> List.map (\y -> ( x, y )))


type MoveDirection
    = Left
    | Right
    | Up
    | Down


slideTilesInDirection : MoveDirection -> List Tile -> List Tile
slideTilesInDirection direction tiles =
    slideTilesInDirectionHelp direction (tilesToLoL tiles)
        |> lolToTiles


slideTilesInDirectionHelp : MoveDirection -> List (List (Maybe Tile)) -> List (List (Maybe Tile))
slideTilesInDirectionHelp direction lol =
    case direction of
        Left ->
            lol
                |> slideRowsLeft

        Right ->
            lol
                |> reverseRows
                |> slideRowsLeft
                |> reverseRows

        Up ->
            lol
                |> LE.transpose
                |> slideRowsLeft
                |> LE.transpose

        Down ->
            lol
                |> LE.transpose
                |> reverseRows
                |> slideRowsLeft
                |> reverseRows
                |> LE.transpose


reverseRows lol =
    List.map List.reverse lol


lolToTiles : List (List (Maybe Tile)) -> List Tile
lolToTiles lol =
    List.indexedMap
        (\y row ->
            List.indexedMap
                (\x mt ->
                    Maybe.map
                        (\t ->
                            { t | pos = ( x, y ) }
                        )
                        mt
                )
                row
                |> List.filterMap identity
        )
        lol
        |> List.concat


slideRowsLeft : List (List (Maybe Tile)) -> List (List (Maybe Tile))
slideRowsLeft lol =
    List.map slideRowLeft lol


slideRowLeft : List (Maybe Tile) -> List (Maybe Tile)
slideRowLeft row =
    let
        front : List Tile
        front =
            List.filterMap identity row

        merged : List Tile
        merged =
            merge front

        padding =
            List.repeat (List.length row - List.length merged) Nothing
    in
    List.map Just merged ++ padding


merge : List Tile -> List Tile
merge tiles =
    case tiles of
        x :: y :: rest ->
            if x.value == y.value then
                { x | value = x.value * 2 } :: merge rest

            else
                x :: merge (y :: rest)

        _ ->
            tiles


tilesToLoL : List Tile -> List (List (Maybe Tile))
tilesToLoL tiles =
    let
        findTileAtGridPos gridPos =
            LE.find (\t -> t.pos == gridPos) tiles
    in
    List.range 0 3
        |> List.map (\y -> List.range 0 3 |> List.map (\x -> findTileAtGridPos ( x, y )))


view model =
    div []
        [ viewGlobalStyles
        , div [ style "padding" "10px" ] [ viewGrid model.tiles ]
        ]


viewGlobalStyles =
    Html.node "style" [] [ text globalStyleText ]


globalStyleText =
    """
       * {
            box-sizing: border-box;
            margin: 0;
            padding: 0;
        }
        body, html {
            --height: 100%;
            min-height: 100%;
            min-width: 100%;
            --margin: 0;
            display: flex;
            justify-content: safe center;
            align-items: safe center;
        }
    """


viewGrid tiles =
    div
        [ HA.style "display" "grid"
        , HA.style "grid" "repeat(4, 100px) / repeat(4, 100px)"
        , style "background" "#444"
        , style "padding" "5px"
        , style "border-radius" "10px"
        ]
        (List.map viewBackgroundGridItem allPositions
            ++ List.map viewGridItem tiles
        )


viewGridItem tile =
    let
        gridPos =
            tile.pos

        value =
            tile.value
    in
    div
        [ gridAreaFromXY gridPos
        , style "padding" "5px"

        --, style "translate" "300%"
        , style "transition" "translate 200ms ease-in"
        ]
        [ div
            [ style "" ""
            , style "width" "100%"
            , style "height" "100%"
            , style "border-radius" "10px"
            , style "display" "grid"
            , style "place-items" "center"
            , style "background" "#222"
            , style "color" "#eee"
            , style "font-size" "48px"
            ]
            --[ text (Debug.toString gp) ]
            [ text (String.fromInt value) ]

        --[]
        ]


viewBackgroundGridItem xy =
    div
        [ gridAreaFromXY xy
        , style "background" "#444"
        , style "padding" "5px"

        --, if List.member xy [ ( 0, 0 ), ( 0, 2 ), ( 0, 1 ) ] then
        --    style "translate" "100%"
        --
        --  else
        --    style "" ""
        ]
        [ div
            [ style "width" "100%"
            , style "height" "100%"
            , style "display" "grid"
            , style "place-content" "center"
            , style "background" "#888"
            , style "border-radius" "10px"
            ]
            --[ text (Debug.toString xy) ]
            []
        ]


gridAreaFromXY ( x, y ) =
    style "grid-area" (String.fromInt (y + 1) ++ "/" ++ String.fromInt (x + 1))
