module World where

import Matrix exposing (Matrix)
import Random

type alias Tile =
    { elevation : Float
    , temperature : Float
    }

tile : Tile
tile =
    { elevation = 0.0
    , temperature = 0.0
    }

initialMap = Matrix.matrix 100 100 (always tile)

type alias World =
    { seaLevel : Float
    , seed : Int
    }

defaultSeaLevel = 0.2

world : Int -> World
world seed =
    { seed = seed
    , seaLevel = defaultSeaLevel
    }