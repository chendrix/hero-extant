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

import World exposing (World, Tile, defaultSeaLevel, initialMap, world)
import Temperature
import Elevation
import Native.Now as Now

import Html
import StartApp

main =
    StartApp.start { model = model, view = view, update = update }


model =
    world <| Random.initialSeed Native.Now.loadTime


generateMap : World -> Matrix Tile
generateMap world =
    (world, world.seed, initialMap)
    |> Elevation.generate
    |> Temperature.generate
    |> (\(_, _, map) -> map)


view address model =
    (render (512,512) generateMap address model)
    |> Html.fromElement


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
    = NewSeed Seed
    | SeaLevel Float


update : Action -> World -> World
update msg model =
    case msg of
        NewSeed seed ->
            { model | seed <- seed }

        SeaLevel seaLevel ->
            { model | seaLevel <- seaLevel }