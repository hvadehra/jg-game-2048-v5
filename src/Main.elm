module Main exposing (main)

import Html exposing (..)
import Html.Attributes as HA exposing (style)


main =
    div []
        [ viewGlobalStyles
        , div [ style "padding" "10px" ] [ viewGrid ]
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


viewGrid =
    div
        [ HA.style "display" "grid"
        , HA.style "grid" "repeat(4, 100px) / repeat(4, 100px)"
        , style "background" "#444"
        , style "padding" "5px"
        , style "border-radius" "10px"
        ]
        (viewBackgroundGridItems
            ++ viewGridItems
        )


viewBackgroundGridItems =
    List.map viewBackgroundGridItem gps


type alias GP =
    ( Int, Int )


type alias Tile =
    { gp : GP
    , val : Int
    }


tiles : List Tile
tiles =
    [ { gp = ( 0, 0 ), val = 2 }
    , { gp = ( 2, 3 ), val = 4 }
    ]


viewGridItems =
    List.map viewGridItem tiles


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
