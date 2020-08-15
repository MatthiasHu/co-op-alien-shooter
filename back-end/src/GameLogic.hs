module GameLogic
 ( Vec(..)
 , Game()
 , startGame
 , PlayerId
 , PlayerInput()
 , tickGame
 , DrawCommand(..)
 , drawGame
 ) where


import Data.Word


data Vec = Vec
  { x :: Float
  , y :: Float
  }

data Game = Game
  { time :: Int
  }

startGame :: Game
startGame = Game 0

newtype PlayerId = PlayerId Int

data PlayerInput = PlayerInput
  { leftPressed :: Bool
  , rightPressed :: Bool
  , upPressed :: Bool
  , downPressed :: Bool
  , shootPressed :: Bool
  }

noPlayerInput :: PlayerInput
noPlayerInput = PlayerInput False False False False False

tickGame :: (PlayerId -> PlayerInput) -> Game -> Game
tickGame inputs g = Game
  { time = time g + 1
  }

type ImageId = Word16

data DrawCommand = DrawCommand
  { imageId :: ImageId
  , drawPosition :: Vec
  }

drawGame :: Game -> [DrawCommand]
drawGame g = []
  ++ [ DrawCommand 0 (Vec 0 0) ]
