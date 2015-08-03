module Elevation (generate, belowSeaLevel, elevationAboveSeaLevel) where

import Matrix exposing (Matrix)
import Random exposing (Seed)
import World exposing (Tile, World)


belowSeaLevel : World -> Tile -> Bool
belowSeaLevel world tile =
    tile.elevation < world.seaLevel

elevationAboveSeaLevel : World -> Tile -> Float
elevationAboveSeaLevel world tile =
    tile.elevation - world.seaLevel

all : List (Maybe a) -> Maybe (List a)
all =
    List.foldr (
        \maybeItem total ->
            case total of
                Nothing -> Nothing
                Just list ->
                    case maybeItem of
                        Nothing -> Nothing
                        Just item -> Just (item :: list)
    ) (Just [])


generate : (World, Seed, Matrix Tile) -> (World, Seed, Matrix Tile)
generate (world, seed, input) =
    let
        roughness = 100.0 / 20
        elevation = 100.0 / 200
        w = Matrix.colCount input
        h = Matrix.rowCount input

        divideWorld : Int -> Int -> Int -> Int -> Float -> Float -> (Seed,Matrix { a | elevation : Float }) -> (Seed,Matrix { a | elevation : Float })
        divideWorld x1 y1 x2 y2 roughness midinit (seed, map) =
            let
                w' = x2 - x1
                h' = y2 - y1
                midx = (x1 + x2) // 2
                midy = (y1 + y2) // 2

                d = (((w' + h' |> toFloat) / 2) / (w + h |> toFloat))
                (random1,seed') = Random.generate (Random.float 0 1) seed
                d' = d * (random1 * 2 - 1) * roughness

                getElevation (x,y) matrix =
                    Maybe.map (.elevation) (Matrix.get (Matrix.loc x y) matrix)

                setElevation (x,y) v matrix =
                    Matrix.update (Matrix.loc x y) (\r -> { r | elevation <- v }) matrix

                calculateMidpoint extra destination inputLocs map' =
                    let
                        allElevations =
                            List.map ((flip getElevation) map') inputLocs
                            |> all
                        valueComputer inputs = extra + (List.sum inputs) / (toFloat <| List.length inputs)
                    in
                        allElevations
                        |> Maybe.map (\inputs ->
                            setElevation destination (valueComputer inputs) map'
                        )
                        |> Maybe.withDefault map'
            in
                if w' > 1 || h' > 1
                then
                    map
                    |> calculateMidpoint 0 (midx,y1) [ (x1,y1), (x2,y1) ]
                    |> calculateMidpoint 0 (midx,y2) [ (x1,y2), (x2,y2) ]
                    |> calculateMidpoint 0 (x1,midy) [ (x1,y1), (x1,y2) ]
                    |> calculateMidpoint 0 (x2,midy) [ (x2,y1), (x2,y2) ]
                    |> calculateMidpoint d' (midx,midy) [ (x1,y1), (x1,y2), (x2,y1), (x2,y2) ]
                    |> (\map' -> if midinit > -1 then setElevation (midx,midy) midinit map' else map')
                    |> (,) seed'
                    |> divideWorld x1 y1 midx midy roughness -1
                    |> divideWorld midx y1 x2 midy roughness -1
                    |> divideWorld x1 midy midx y2 roughness -1
                    |> divideWorld midx midy x2 y2 roughness -1

                else (seed',map)
    in
        (seed, input)
        |> divideWorld 0 0 w h roughness elevation
        |> (\(seed', map') -> (world, seed', map'))
