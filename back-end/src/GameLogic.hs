module GameLogic
 ( Vec(..)
 , Game()
 , startGame
 , PlayerId
 , PlayerInput()
 , noPlayerInput
 , tickGame
 , DrawCommand(..)
 , drawGame
 ) where


import Data.Word


data Vec = Vec
  { vecX :: Float
  , vecY :: Float
  }

data Game = Game
  { time :: Int
  , aliens :: [Alien]
  }

data Alien = Alien
  { alienPos :: Vec
  }

startGame :: Game
startGame = Game 0 []

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
tickGame inputs =
      (\g -> g {time = time g + 1})
  >.> (\g -> g {aliens = map tickAlien (aliens g)})
  >.> (\g -> g {aliens = aliens g ++ spawningAliens g})
  >.> (\g -> g {aliens = filter (not . alienDead) (aliens g)})
  where
    f1 >.> f2 = \x -> f1 (f2 x)

spawningAliens :: Game -> [Alien]
spawningAliens g =
  if time g `mod` 10 == 0
  then let t = fromIntegral (time g `div` 10)
           fractionalPart x = x - fromIntegral (round x)
       in [Alien (Vec (fractionalPart (t * 0.39) * 1.6) 1.5)]
  else []

tickAlien :: Alien -> Alien
tickAlien (Alien (Vec x y)) = Alien (Vec x (y - 0.01))

alienDead :: Alien -> Bool
alienDead a = vecY (alienPos a) < -1.5

type ImageId = Word16

data DrawCommand = DrawCommand
  { imageId :: ImageId
  , drawPosition :: Vec
  }

drawGame :: Game -> [DrawCommand]
drawGame g = []
  ++ [ DrawCommand 1 (alienPos a) | a <- aliens g ]
