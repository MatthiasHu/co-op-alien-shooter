module Serialization
  ( serializeDrawCommand
  , deserializePlayerInput
  ) where


import Data.ByteString.Builder
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits

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

deserializePlayerInput :: BS.ByteString -> Maybe PlayerInput
deserializePlayerInput bs = case BS.unpack bs of
  a : _ -> Just PlayerInput
    { leftPressed  = (testBit a 0)
    , rightPressed = (testBit a 1)
    , downPressed  = (testBit a 2)
    , upPressed    = (testBit a 3)
    , shootPressed = (testBit a 4)
    }
  _ -> Nothing
