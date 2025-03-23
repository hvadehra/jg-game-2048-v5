module Main exposing (main, suite)

import Html exposing (..)
import Html.Attributes as HA exposing (style)
import List.Extra as LE
import Random exposing (Generator)
import Random.List
import Maybe exposing (Maybe)
import Test exposing (..)
import Expect exposing (Expectation)

randomTilesCount =
    4


main =
    let
        initialTiles : List Tile
        initialTiles =
            Random.step (randomTiles allGPs) (Random.initialSeed 2)
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

        --|> slideTilesInDirection Left
        --|> slideTilesInDirection Right
        --|> slideTilesInDirection Left
        --|> slideTilesInDirection Up
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
        |> List.concatMap (\y -> List.range 0 3 |> List.map (\x -> ( x, y )))


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
        front : List Tile
        front =
            List.filterMap identity row

        merged : List Tile
        merged =
            merge front
                |> Debug.log "tiles"

        padding =
            List.repeat (List.length row - List.length front) Nothing
                |> Debug.log "padding"
    in
    List.map Just merged ++ padding


merge : List Tile -> List Tile
merge tiles =
    case tiles of
        x :: y :: rest ->
            if x.val == y.val then
                { x | val = x.val * 2 } :: merge rest

            else
                x :: merge (y :: rest)

        _ ->
            tiles


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

suite : Test
suite =
    let
       makeTile ((x,y), val) = Maybe.map (Tile (x,y)) val
       printRow row
           = List.map printTile row |>
             String.join " "
       printTile tile
           = case tile of
               Just {gp,val} -> (String.fromInt val)
               Nothing -> "."
       parseGrid rows
           = List.map (String.words) rows |>
             List.concat |>
             List.map String.toInt |>
             LE.zip allGPs |>
             List.map makeTile |>
             List.filterMap identity
       prettyPrintGrid tiles
           = tilesToLOL tiles |>
             List.map printRow

    in
    describe "All tests"
        [ test "parser" <|
            \_ -> parseGrid [
                            ". 1 . .",
                            ". . 2 3",
                            "4 . . .",
                            ". 5 6 ."
                           ] |>
            Expect.equal [Tile (1,0) 1, Tile (2,1) 2, Tile (3,1) 3, Tile (0,2) 4, Tile (1,3) 5, Tile (2,3) 6]
        , test "printer" <|
            \_ -> prettyPrintGrid [Tile (1,0) 1, Tile (2,1) 2, Tile (3,1) 3, Tile (0,2) 4, Tile (1,3) 5, Tile (2,3) 6] |>
            Expect.equal [
                ". 1 . .",
                ". . 2 3",
                "4 . . .",
                ". 5 6 ."
            ]
         , describe "empty grid" <|
            let
                checkDirection direction =
                    test (Debug.toString direction) <|
                        \_ -> slideTilesInDirection direction []
                            |> Expect.equal []
            in
                [ checkDirection Left,
                  checkDirection Right,
                  checkDirection Up,
                  checkDirection Down
                ]
        , describe "Slide without merges" <|
                let grid = parseGrid [
                                     "1 . 3 .",
                                     ". 2 . .",
                                     "4 . 5 .",
                                     ". 6 . 7"
                                     ]
                in
                [ test "Left" <|
                    \_ -> slideTilesInDirection Left grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                            "1 3 . .",
                            "2 . . .",
                            "4 5 . .",
                            "6 7 . ."
                        ]
                , test "Right" <|
                    \_ -> slideTilesInDirection Right grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                            ". . 1 3",
                            ". . . 2",
                            ". . 4 5",
                            ". . 6 7"
                        ]
                , test "Down" <|
                    \_ -> slideTilesInDirection Down grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                            ". . . .",
                            ". . . .",
                            "1 2 3 .",
                            "4 6 5 7"
                        ]
                , test "Up" <|
                    \_ -> slideTilesInDirection Up grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                            "1 2 3 7",
                            "4 6 5 .",
                            ". . . .",
                            ". . . ."
                        ]
                ]
        , describe "Slide with merges" <|
                let grid = parseGrid [
                            ". . 4 .",
                            ". 2 2 4",
                            "4 2 2 .",
                            ". 4 . ."
                            ]
                in
                [ test "Left" <|
                    \_ -> slideTilesInDirection Left grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                            "4 . . .",
                            "4 4 . .",
                            "4 4 . .",
                            "4 . . ."
                        ]
                , test "Right" <|
                    \_ -> slideTilesInDirection Right grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                          ". . . 4",
                          ". . 4 4",
                          ". . 4 4",
                          ". . . 4"
                        ]
                , test "Down" <|
                    \_ -> slideTilesInDirection Down grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                          ". . . .",
                          ". . . .",
                          ". 4 4 .",
                          "4 4 4 4"
                        ]
                , test "Up" <|
                    \_ -> slideTilesInDirection Up grid |>
                        prettyPrintGrid |>
                        Expect.equal [
                          "4 4 4 4",
                          ". 4 4 .",
                          ". . . .",
                          ". . . ."
                        ]
                ]
        ]
