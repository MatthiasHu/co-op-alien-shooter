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
import Linear
import Control.Lens


type Vec = V2 Float

norm :: Vec -> Float
norm (V2 x y) = sqrt (x^2 + y^2)

data Game = Game
  { time :: Int
  , aliens :: [Alien]
  , players :: PlayersMap
  , bullets :: [Bullet]
  }

type PlayersMap = M.Map PlayerId Player

data Alien = Alien
  { alienPos :: Vec
  }

data Player = Player
  { playerPos :: Vec
  , playerVel :: Vec
  , playerShootCharge :: Float
  }

data Bullet = Bullet
  { bulletPos :: Vec
  }

startGame :: Game
startGame = Game
  { time = 0
  , aliens = []
  , players = M.empty
  , bullets = []
  }

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
  >.> (\g -> g {bullets = map tickBullet (bullets g)})
  >.> (\g -> g {bullets = bullets g ++ spawningBullets g playerInputs})
  >.> (\g -> g {bullets = filter (not . bulletDead) (bullets g)})
  where
    f1 >.> f2 = \x -> f1 (f2 x)

spawningAliens :: Game -> [Alien]
spawningAliens g =
  if time g `mod` 10 == 0
  then let t = fromIntegral (time g `div` 10)
           fractionalPart x = x - fromIntegral (round x)
           alien = Alien (V2 (fractionalPart (t * 0.39) * 1.6) 1.5)
       in [alien]
  else []

tickAlien :: Alien -> Alien
tickAlien (Alien (V2 x y)) = Alien (V2 x (y - 0.01))

alienDead :: Alien -> Bool
alienDead a = alienPos a ^. _x < -1.5

tickPlayers :: M.Map PlayerId PlayerInput -> PlayersMap -> PlayersMap
tickPlayers playerInputs playersMap =
  remainingPlayers `M.union` newPlayers
  where
    remainingPlayers =
      M.intersectionWith tickPlayer playersMap playerInputs
    newPlayers = M.fromSet (const newPlayer)
      ((M.keysSet playerInputs) S.\\ (M.keysSet playersMap))

newPlayer :: Player
newPlayer = Player
  { playerPos = (V2 0 (-0.6))
  , playerVel = (V2 0 0)
  , playerShootCharge = 0
  }

tickPlayer :: Player -> PlayerInput -> Player
tickPlayer p input =
  p { playerPos = V2 px' py'
    , playerVel = V2 vx' vy'
    , playerShootCharge = playerShootCharge p + 0.1
    }
  where
    px' = clamp (playerPos p ^. _x + vx')
    py' = clamp (playerPos p ^. _y + vy')
    clamp x = max (-1) (min 1 x)
    vx' = (playerVel p ^. _x) * 0.8 + lr * 0.01
    vy' = (playerVel p ^. _y) * 0.8 + du * 0.01
    lr = if leftPressed  input then -1 else 0
       + if rightPressed input then  1 else 0
    du = if downPressed  input then -1 else 0
       + if upPressed    input then  1 else 0

letPlayersShoot :: Game -> M.Map PlayerId PlayerInput -> Game
letPlayersShoot g playerInputs = g
  { bullets = bullets g ++ map newBullet shooting
--  , players = ...
  }
  where
    shooting = shootingPlayers g playerInputs
    newBullet playerId = Bullet (playerPos ((players g) M.! playerId))
--    ... M.adjust (\p -> p {playerShootCharge = 0})

shootingPlayers :: Game -> M.Map PlayerId PlayerInput -> [PlayerId]
shootingPlayers g playerInputs = do
  (playerId, input) <- M.assocs playerInputs
  if shootPressed input then [playerId] else []

spawningBullets :: Game -> M.Map PlayerId PlayerInput -> [Bullet]
spawningBullets g playerInputs = do
  (playerId, input) <- M.assocs playerInputs
  let pos = playerPos (players g M.! playerId)
  if shootPressed input
  then [Bullet pos]
  else []

tickBullet :: Bullet -> Bullet
tickBullet (Bullet pos) = Bullet (pos + (V2 0 0.02))

bulletDead :: Bullet -> Bool
bulletDead (Bullet pos) = pos ^. _y > 1.5

type ImageId = Word16

data DrawCommand = DrawCommand
  { imageId :: ImageId
  , drawPosition :: Vec
  }

drawGame :: Game -> [DrawCommand]
drawGame g = []
  ++ [ DrawCommand 1 (alienPos a) | a <- aliens g ]
  ++ [ DrawCommand 0 (playerPos p) | p <- M.elems (players g) ]
  ++ [ DrawCommand 2 (bulletPos p) | p <- bullets g ]
