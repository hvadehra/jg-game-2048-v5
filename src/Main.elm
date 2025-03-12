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

        --|> slideTilesInDirection Left
        --|> slideTilesInDirection Up
        --|> slideTilesInDirection Right
        --|> slideTilesInDirection Left
        --|> slideTilesInDirection Down
        --|> slideTilesInDirection Down
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


type SlideDirection
    = Left
    | Right
    | Up
    | Down


slideTilesInDirection : SlideDirection -> List Tile -> List Tile
slideTilesInDirection direction tiles =
    slideTilesInDirectionHelp direction (tilesToLOL tiles)
        |> lolToTiles


slideTilesInDirectionHelp : SlideDirection -> List (List (Maybe Tile)) -> List (List (Maybe Tile))
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
                            { t | gp = ( x, y ) }
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
        front =
            List.filterMap identity row

        tails =
            List.repeat (List.length row - List.length front) Nothing
    in
    List.map Just front ++ tails


tilesToLOL : List Tile -> List (List (Maybe Tile))
tilesToLOL tiles =
    let
        findTileAtGP gp =
            LE.find (\t -> t.gp == gp) tiles
    in
    List.range 0 3
        |> List.map (\y -> List.range 0 3 |> List.map (\x -> findTileAtGP ( x, y )))


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
