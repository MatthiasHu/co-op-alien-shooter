module GameLogic
 ( Vec(..)
 , Game()
 , startGame
 , PlayerId
 , PlayerInput(..)
 , noPlayerInput
 , tickGame
 , DrawCommand(..)
 , drawGame
 ) where


import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word


data Vec = Vec
  { vecX :: Float
  , vecY :: Float
  }

data Game = Game
  { time :: Int
  , aliens :: [Alien]
  , players :: PlayersMap
  }

type PlayersMap = M.Map PlayerId Player

data Alien = Alien
  { alienPos :: Vec
  }

data Player = Player
  { playerPos :: Vec
  , playerVel :: Vec
  }

startGame :: Game
startGame = Game 0 [] M.empty

type PlayerId = Int

data PlayerInput = PlayerInput
  { leftPressed :: Bool
  , rightPressed :: Bool
  , upPressed :: Bool
  , downPressed :: Bool
  , shootPressed :: Bool
  }

noPlayerInput :: PlayerInput
noPlayerInput = PlayerInput False False False False False

tickGame :: (M.Map PlayerId PlayerInput) -> Game -> Game
tickGame playerInputs =
      (\g -> g {time = time g + 1})
  >.> (\g -> g {aliens = map tickAlien (aliens g)})
  >.> (\g -> g {aliens = aliens g ++ spawningAliens g})
  >.> (\g -> g {aliens = filter (not . alienDead) (aliens g)})
  >.> (\g -> g {players = tickPlayers playerInputs (players g)})
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

tickPlayers :: M.Map PlayerId PlayerInput -> PlayersMap -> PlayersMap
tickPlayers playerInputs playersMap =
  remainingPlayers `M.union` newPlayers
  where
    remainingPlayers =
      M.intersectionWith tickPlayer playersMap playerInputs
    newPlayers = M.fromSet (const newPlayer)
      ((M.keysSet playerInputs) S.\\ (M.keysSet playersMap))

newPlayer :: Player
newPlayer = Player (Vec 0 (-0.6)) (Vec 0 0)

tickPlayer :: Player -> PlayerInput -> Player
tickPlayer p input =
  p { playerPos = Vec px' py'
    , playerVel = Vec vx' vy'
    }
  where
    px' = clamp (vecX (playerPos p) + vx')
    py' = clamp (vecY (playerPos p) + vy')
    clamp x = max (-1) (min 1 x)
    vx' = vecX (playerVel p) * 0.8 + lr * 0.01
    vy' = vecY (playerVel p) * 0.8 + du * 0.01
    lr = if leftPressed  input then -1 else 0
       + if rightPressed input then  1 else 0
    du = if downPressed  input then -1 else 0
       + if upPressed    input then  1 else 0

type ImageId = Word16

data DrawCommand = DrawCommand
  { imageId :: ImageId
  , drawPosition :: Vec
  }

drawGame :: Game -> [DrawCommand]
drawGame g = []
  ++ [ DrawCommand 1 (alienPos a) | a <- aliens g ]
  ++ [ DrawCommand 0 (playerPos p) | p <- M.elems (players g) ]
