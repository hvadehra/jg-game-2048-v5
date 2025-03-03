module Main exposing (main)

import Html exposing (..)
import Html.Attributes as HA exposing (style)
import Random exposing (Generator)


main =
    let
        initialTiles : List Tile
        initialTiles =
            Random.step randomInitialTiles (Random.initialSeed 2)
                |> Tuple.first
                |> Debug.log ""
    in
    view { tiles = initialTiles }


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
        (List.map viewBackgroundGridItem gps
            ++ List.map viewGridItem tiles
        )


type alias GP =
    ( Int, Int )


type alias Tile =
    { gp : GP
    , val : Val
    }


type alias Val =
    Int


randomInitialTiles : Generator (List Tile)
randomInitialTiles =
    let
        randomGPs =
            randomAvailableGPs 2 gps []
    in
    Debug.todo ""


randomAvailableGPs : Int -> List GP -> List GP -> Generator (List GP)
randomAvailableGPs maxGP fromGPs acc =
    if maxGP <= 0 then
        Random.constant acc

    else
        case fromGPs of
            [] ->
                Random.constant acc

            x :: xs ->
                Random.uniform x xs
                    |> Random.andThen
                        (\gp ->
                            randomAvailableGPs (maxGP - 1) (List.filter (\y -> y == gp) (x :: xs)) (gp :: acc)
                        )


randomGP : Generator GP
randomGP =
    Random.uniform ( 0, 0 ) (List.drop 1 gps)


randomInitialVal : Generator Val
randomInitialVal =
    Random.uniform 2 [ 4 ]


initTile : GP -> Val -> Tile
initTile gp val =
    Tile gp val


gps =
    List.range 0 3
        |> List.concatMap (\x -> List.range 0 3 |> List.map (\y -> ( x, y )))


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
