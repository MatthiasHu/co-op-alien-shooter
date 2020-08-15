module Serialization
  ( serializeDrawCommand
  ) where


import Data.ByteString.Builder

import GameLogic


serializeDrawCommand :: DrawCommand -> Builder
serializeDrawCommand (DrawCommand imageId drawPosition) =
     word16BE imageId
  <> serializePosition drawPosition

serializePosition :: Vec -> Builder
serializePosition (Vec x y) =
     (serializeCoordinate x)
  <> (serializeCoordinate y)
  where
    -- Use 16 bits for coordinates,
    -- covering the drawing area width/height twice.
    serializeCoordinate c = word16BE . round $ ((c + 2)/4) * (2**16)
