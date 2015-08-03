module World where

import Matrix exposing (Matrix)
import Random exposing (Seed)

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
    , seed : Seed
    }

defaultSeaLevel = 0.2

world : Seed -> World
world seed =
    { seed = seed
    , seaLevel = defaultSeaLevel
    }