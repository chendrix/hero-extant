module Temperature where

import Matrix exposing (Matrix)
import Random exposing (Seed)

import World exposing (World, Tile)
import Elevation exposing (belowSeaLevel, elevationAboveSeaLevel)

generate : (World, Seed, Matrix Tile) -> (World, Seed, Matrix Tile)
generate (world, seed, map) =
  let
    w = Matrix.colCount map
    h = Matrix.rowCount map
    generateBand location curTile =
      let
        rowNum = Matrix.row location
        colNum = Matrix.col location
        bandRand = 7
        bandTemp =
          (toFloat rowNum) / (toFloat h)
          |> max 0.075
        temperature =
          if  | belowSeaLevel world curTile -> bandTemp * 0.7
              | otherwise -> bandTemp * (1.0 - (elevationAboveSeaLevel world curTile))
      in
        { curTile | temperature <- temperature }
    newMap =
      Matrix.mapWithLocation generateBand map
  in
    (world, seed, newMap)

  --for (int bandy = 0; bandy < h; bandy += TEMPERATURE_BAND_RESOLUTION) {

  --          // Generate band
  --          final int bandrange = 7;

  --          double bandtemp;
  --          switch (this.hemisphere) {
  --              case North:
  --                  // 0, 0.5, 1
  --                  bandtemp = (double) bandy / h;
  --                  break;
  --              case Equator:
  --                  // 0, 1, 0
  --                  if (bandy < h / 2) {
  --                      bandtemp = (double) bandy / h;
  --                      bandtemp = bandtemp * 2.0;
  --                  } else {
  --                      bandtemp = 1.0 - (double) bandy / h;
  --                      bandtemp = bandtemp * 2.0;
  --                  }
  --                  break;
  --              case South:
  --                  // 1, 0.5, 0
  --                  bandtemp = 1.0 - (double) bandy / h;
  --                  break;
  --              default:
  --                  bandtemp = 0;
  --                  break;
  --          }
  --          bandtemp = Math.max(bandtemp, 0.075);

  --          final int[] band = new int[w];
  --          for (int x = 0; x < w; x++) {
  --              band[x] = bandy;
  --          }

  --          // Randomize
  --          double dir = 1.0;
  --          double diradj = 1;
  --          double dirsin = r.nextDouble() * 7 + 1;
  --          for (int x = 0; x < w; x++) {
  --              band[x] = (int) (band[x] + dir);
  --              dir = dir + r.nextDouble() * (Math.sin(dirsin * x) * diradj);
  --              if (dir > bandrange) {
  --                  diradj = -1;
  --                  dirsin = r.nextDouble() * 7 + 1;
  --              }
  --              if (dir < -bandrange) {
  --                  diradj = 1;
  --                  dirsin = r.nextDouble() * 7 + 1;
  --              }
  --          }

  --          for (int x = 0; x < w; x++) {
  --              for (int y = 0; y < h; y++) {

  --                  double elevation = getElevation(x, y);
  --                  if (elevation < WorldGenerator.SEA_LEVEL) {
  --                      // Water tiles
  --                      if (y > band[x]) {
  --                          setTemperature(x, y, bandtemp * 0.7);
  --                      }
  --                  } else {
  --                      // Land tiles
  --                      if (y > band[x]) {
  --                          setTemperature(x, y, bandtemp * (1.0 - (elevation - WorldGenerator.SEA_LEVEL)));
  --                      }
  --                  }
  --              }
  --          }