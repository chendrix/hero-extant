import Graphics.Collage as C exposing (..)
import Graphics.Element as E exposing (..)
import Color exposing (..)
import Matrix exposing (Matrix)
import Random exposing (Seed)
import Text
import Debug
import Maybe exposing (map, withDefault, andThen, Maybe (..) )
import Array
import Time

import Result
import String

import World exposing (World, Tile, defaultSeaLevel, initialMap, world)
import Random
import Temperature
import Elevation
import Native.Now as Now

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp

main =
    StartApp.start { model = model, view = view, update = update }


model =
    world <| Native.Now.loadTime


generateMap : World -> Matrix Tile
generateMap world =
    (world, Random.initialSeed world.seed, initialMap)
    |> Elevation.generate
    |> Temperature.generate
    |> (\(_, _, map) -> map)


view : Signal.Address Action -> World -> Html
view address model =
    let
        map =
            (render (512,512) generateMap address model)
            |> Html.fromElement

        seedInput =
            input
                [ id "update-seed"
                , type' "number"
                , value (model.seed |> toString)
                , onInput address (\strVal ->
                    String.toInt strVal
                    |> Result.toMaybe
                    |> Maybe.withDefault model.seed
                    |> NewSeed
                    )
                ]
                []
        seaLevelSlider =
            input
                [ id "update-seaLevel"
                , type' "range"
                , value (model.seaLevel |> toString)
                , onInput address (\strVal ->
                    String.toFloat strVal
                    |> Result.toMaybe
                    |> Maybe.withDefault model.seaLevel
                    |> SeaLevel
                    )
                , Html.Attributes.min "0"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.05"
                ]
                []
    in
        div []
            [ seedInput
            , seaLevelSlider
            , map
            ]

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))


render : (Int,Int) -> (World -> Matrix Tile)-> Signal.Address Action  -> World -> Element
render (mapWidth,mapHeight) mapper address world =
    let
        map = mapper world
        boxWidth = (toFloat mapWidth) / (toFloat <| Matrix.colCount map)
        boxHeight = (toFloat mapHeight) / (toFloat <| Matrix.rowCount map)
        box = C.rect boxWidth boxHeight

        colorFor tile =
            let
                belowSeaLevel = Elevation.belowSeaLevel world tile
                elevation = tile.elevation
                c val = floor <| 255 * val
            in
                if  | belowSeaLevel -> rgb 0 0 (c <| 0.5 * (elevation) + 0.5)
                    | tile.temperature < 0.15 -> rgb (c <| 0.25 + 0.75 * elevation) (c <| 0.25 + 0.75 * elevation) (c <| 0.25 + 0.75 * elevation)
                    | otherwise -> rgb (floor <| 255 * 0.75 * elevation) (floor <| 255 * 0.75 * elevation) 0

        cell tile =
            [ C.filled (colorFor tile) box ]
            |> C.collage (floor boxWidth) (floor boxHeight)
    in
        map
        |> Matrix.map cell
        |> Matrix.toList
        |> List.map (flow right)
        |> flow down


type Action
    = NewSeed Int
    | SeaLevel Float


update : Action -> World -> World
update msg model =
    case msg of
        NewSeed seed ->
            { model | seed <- seed }

        SeaLevel seaLevel ->
            { model | seaLevel <- seaLevel }