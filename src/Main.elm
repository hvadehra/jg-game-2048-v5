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
            Random.step (randomTiles allGPs) (Random.initialSeed 2)
                |> Tuple.first

        --|> Debug.log ""
        --        |> always initialTiles2
        --
        --initialTiles2 =
        --    [ Tile ( 3, 0 ) 4
        --    , Tile ( 2, 0 ) 2
        --    ]
        --_ =
        --    List.length initialTiles |> Debug.log "len"
    in
    view
        { tiles =
            initialTiles
                |> slideBoardLeft

        --|> slideLeft
        --|> slideRight
        --|> Debug.log "foo"
        }


type alias GP =
    ( Int, Int )


type alias Tile =
    { gp : GP
    , val : Val
    }


type alias Val =
    Int


randomTiles : List GP -> Generator (List Tile)
randomTiles emptyGPs =
    randomAvailableGPsUpto randomTilesCount emptyGPs
        |> Random.andThen randomTilesFromGPs


randomAvailableGPsUpto : Int -> List GP -> Generator (List GP)
randomAvailableGPsUpto maxGP availableGPs =
    Random.List.shuffle availableGPs
        |> Random.map (List.take maxGP)


randomTilesFromGPs : List GP -> Generator (List Tile)
randomTilesFromGPs gps =
    let
        len =
            List.length gps
    in
    Random.list len randomInitialVal
        |> Random.map
            (\vals ->
                List.map2 Tuple.pair gps vals
                    |> List.map (\( gp, val ) -> initTile gp val)
            )


randomInitialVal : Generator Val
randomInitialVal =
    Random.uniform 2 [ 4 ]


initTile : GP -> Val -> Tile
initTile gp val =
    Tile gp val


allGPs =
    List.range 0 3
        |> List.concatMap (\x -> List.range 0 3 |> List.map (\y -> ( x, y )))


slideBoardLeft : List Tile -> List Tile
slideBoardLeft tiles =
    let
        rotationCount =
            0
    in
    tiles
        |> tilesToLOL
        |> rotate90NTimes rotationCount
        |> slideLeftRows
        |> rotate90NTimes -rotationCount
        |> lolToTiles


lolToTiles : List (List (Maybe Tile)) -> List Tile
lolToTiles lol =
    List.indexedMap
        (\y row ->
            List.indexedMap
                (\x mt ->
                    Maybe.map
                        (\t ->
                            { t | gp = ( x, y ) }
                        )
                        mt
                )
                row
                |> List.filterMap identity
        )
        lol
        |> List.concat


slideLeftRows : List (List (Maybe Tile)) -> List (List (Maybe Tile))
slideLeftRows lol =
    List.map slideLeftRow lol


slideLeftRow : List (Maybe Tile) -> List (Maybe Tile)
slideLeftRow row =
    let
        front =
            List.filterMap identity row

        tails =
            List.repeat (List.length row - List.length front) Nothing
    in
    List.map Just front ++ tails


rotate90 : List (List a) -> List (List a)
rotate90 lol =
    LE.transpose lol
        |> List.map List.reverse


rotate90NTimes : Int -> List (List a) -> List (List a)
rotate90NTimes n lol =
    times n rotate90 lol


times n fn a =
    if n <= 0 then
        a

    else
        times (n - 1) fn (fn a)


tilesToLOL : List Tile -> List (List (Maybe Tile))
tilesToLOL tiles =
    let
        findTileAtGP gp =
            LE.find (\t -> t.gp == gp) tiles
    in
    List.range 0 3
        |> List.map (\y -> List.range 0 3 |> List.map (\x -> findTileAtGP ( x, y )))


slideRight : List Tile -> List Tile
slideRight tiles =
    groupTilesByY tiles
        |> List.map sortTilesByNegativeX
        |> List.map updateTileXByIndexMinus3
        |> List.concat


updateTileXByIndexMinus3 : List Tile -> List Tile
updateTileXByIndexMinus3 tiles =
    tiles
        |> List.indexedMap
            (\i t ->
                let
                    ( _, y ) =
                        t.gp
                in
                { t | gp = ( 3 - i, y ) }
            )


slideLeft tiles =
    -- split by y
    -- sort by x
    -- replace x by index
    let
        newTiles =
            groupTilesByY tiles
                |> List.map sortTilesByX
                |> List.map updateTileXByIndex
                |> List.concat
    in
    newTiles


updateTileXByIndex : List Tile -> List Tile
updateTileXByIndex tiles =
    tiles
        |> List.indexedMap
            (\i t ->
                let
                    ( _, y ) =
                        t.gp
                in
                { t | gp = ( i, y ) }
            )


sortTilesByNegativeX : List Tile -> List Tile
sortTilesByNegativeX tiles =
    List.sortBy
        (\t ->
            let
                ( x, _ ) =
                    t.gp
            in
            -x
        )
        tiles


sortTilesByX : List Tile -> List Tile
sortTilesByX tiles =
    List.sortBy
        (\t ->
            let
                ( x, _ ) =
                    t.gp
            in
            x
        )
        tiles


groupTilesByY : List Tile -> List (List Tile)
groupTilesByY tiles =
    List.range 0 3
        |> List.map
            (\i ->
                List.filter
                    (\t ->
                        let
                            ( _, y ) =
                                t.gp
                        in
                        i == y
                    )
                    tiles
            )


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
        (List.map viewBackgroundGridItem allGPs
            ++ List.map viewGridItem tiles
        )


viewGridItem tile =
    let
        gp =
            tile.gp

        val =
            tile.val
    in
    div
        [ gridAreaFromXY gp
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
            [ text (String.fromInt val) ]

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
