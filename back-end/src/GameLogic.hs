{-# LANGUAGE TemplateHaskell #-}

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
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative


type Vec = V2 Float

norm :: Vec -> Float
norm (V2 x y) = sqrt (x^2 + y^2)

data Game = Game
  { _time :: Int
  , _aliens :: [Alien]
  , _players :: PlayersMap
  , _bullets :: [Bullet]
  }

type PlayersMap = M.Map PlayerId Player

data Alien = Alien
  { _alienPos :: Vec
  }

data Player = Player
  { _playerPos :: Vec
  , _playerVel :: Vec
  , _playerShootCharge :: Float
  }

data Bullet = Bullet
  { _bulletPos :: Vec
  }

startGame :: Game
startGame = Game
  { _time = 0
  , _aliens = []
  , _players = M.empty
  , _bullets = []
  }

type PlayerId = Int

data PlayerInput = PlayerInput
  { _leftPressed :: Bool
  , _rightPressed :: Bool
  , _upPressed :: Bool
  , _downPressed :: Bool
  , _shootPressed :: Bool
  }

noPlayerInput :: PlayerInput
noPlayerInput = PlayerInput False False False False False

$(makeLenses ''Game)
$(makeLenses ''Alien)
$(makeLenses ''Player)
$(makeLenses ''Bullet)
$(makeLenses ''PlayerInput)

tickGame :: (M.Map PlayerId PlayerInput) -> Game -> Game
tickGame playerInputsMap = execState $ do
  time += 1
  let connectedPlayers = M.keysSet playerInputsMap
  removeDisconnectedPlayers connectedPlayers
  spawnNewPlayers connectedPlayers
  let playerInput = playerInputsMapToFunction playerInputsMap
  tickPlayers playerInput
  letPlayersShoot playerInput
  zoom (bullets . each) tickBullet
  bullets %= filter (not . bulletDead)
  get >>= (\g -> aliens %= (++ spawningAliens g))
  zoom (aliens . each) tickAlien
  aliens %= filter (not . alienDead)

playerInputsMapToFunction ::
  M.Map PlayerId PlayerInput -> PlayerId -> PlayerInput
playerInputsMapToFunction m k = M.findWithDefault noPlayerInput k m

spawningAliens :: Game -> [Alien]
spawningAliens g =
  if (g ^. time) `mod` 10 == 0
  then let t = fromIntegral ((g ^. time) `div` 10)
           fractionalPart x = x - fromIntegral (round x)
           alien = Alien (V2 (fractionalPart (t * 0.39) * 1.6) 1.5)
       in [alien]
  else []

tickAlien :: State Alien ()
tickAlien = do
  alienPos += V2 0 (-0.01)

alienDead :: Alien -> Bool
alienDead a = a ^. alienPos . _y < -1.5

removeDisconnectedPlayers :: S.Set PlayerId -> State Game ()
removeDisconnectedPlayers connectedPlayers =
  players %= flip M.restrictKeys connectedPlayers

spawnNewPlayers :: S.Set PlayerId -> State Game ()
spawnNewPlayers connectedPlayers = do
  presentPlayers <- use (players . to M.keysSet)
  let newPlayers = connectedPlayers S.\\ presentPlayers
  players %= (`M.union` M.fromSet (const newPlayer) newPlayers)

newPlayer :: Player
newPlayer = Player
  { _playerPos = (V2 0 (-0.6))
  , _playerVel = (V2 0 0)
  , _playerShootCharge = 0
  }

tickPlayers :: (PlayerId -> PlayerInput) -> State Game ()
tickPlayers playerInput = do
  playerIds <- use (players . to M.keys)
  forM_ playerIds $ \playerId ->
    zoom (players . ix playerId) (tickPlayer (playerInput playerId))

tickPlayer :: PlayerInput -> State Player ()
tickPlayer input = do
  playerVel %= (^* 0.8)
  playerVel += directionsPressed input ^* 0.01
  playerPos <~ liftA2 (^+^) (use playerPos) (use playerVel)
  playerPos %= fmap clamp
  where
    clamp x = max (-1) (min 1 x)

directionsPressed :: PlayerInput -> Vec
directionsPressed input = V2
  (val (input ^. rightPressed) - val (input ^. leftPressed))
  (val (input ^.    upPressed) - val (input ^. downPressed))
  where
    val b = if b then 1 else 0

letPlayersShoot :: (PlayerId -> PlayerInput) -> State Game ()
letPlayersShoot playerInput = do
  playerIds <- use (players . to M.keys)
  newBullets <- execWriterT $ forM playerIds $ \playerId ->
    zoom (players . ix playerId) $
      letOnePlayerShoot (playerInput playerId)
  bullets %= (++ newBullets)

letOnePlayerShoot :: PlayerInput -> WriterT [Bullet] (State Player) ()
letOnePlayerShoot input = do
  charge <- playerShootCharge <+= 0.1
  when (input ^. shootPressed && charge >= 1) $ do
    playerShootCharge .= 0
    use playerPos >>= (tell . (:[]) . Bullet)

tickBullet :: State Bullet ()
tickBullet = bulletPos += (V2 0 0.02)

bulletDead :: Bullet -> Bool
bulletDead (Bullet pos) = pos ^. _y > 1.5

type ImageId = Word16

data DrawCommand = DrawCommand
  { imageId :: ImageId
  , drawPosition :: Vec
  }

drawGame :: Game -> [DrawCommand]
drawGame g = []
  ++ [ DrawCommand 1 (a ^. alienPos) | a <- _aliens g ]
  ++ [ DrawCommand 0 (p ^. playerPos) | p <- M.elems (_players g) ]
  ++ [ DrawCommand 2 (p ^. bulletPos) | p <- _bullets g ]
