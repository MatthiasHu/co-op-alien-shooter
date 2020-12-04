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

data Game = Game
  { _time :: Int
  , _aliens :: [Alien]
  , _players :: PlayersMap
  , _bullets :: [Bullet]
  }

type PlayersMap = M.Map PlayerId Player

data Alien = Alien
  { _alienPos :: Vec
  , _alienDead :: Bool
  }

data Player = Player
  { _playerPos :: Vec
  , _playerVel :: Vec
  , _playerShootCharge :: Float
  }

data Bullet = Bullet
  { _bulletPos :: Vec
  , _bulletDead :: Bool
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
  get >>= (\g -> aliens %= (++ spawningAliens g))
  zoom (aliens . each) tickAlien
  letBulletsHit
  bullets %= filter (not . view bulletDead)
  aliens %= filter (not . view alienDead)

playerInputsMapToFunction ::
  M.Map PlayerId PlayerInput -> PlayerId -> PlayerInput
playerInputsMapToFunction m k = M.findWithDefault noPlayerInput k m

spawningAliens :: Game -> [Alien]
spawningAliens g =
  if (g ^. time) `mod` 20 == 0
  then let t = fromIntegral ((g ^. time) `div` 20)
           fractionalPart x = x - fromIntegral (round x)
           alien = newAlien (V2 (fractionalPart (t * 0.39) * 1.6) 1.5)
       in [alien]
  else []

newAlien :: Vec -> Alien
newAlien pos = Alien
  { _alienPos = pos
  , _alienDead = False
  }

tickAlien :: State Alien ()
tickAlien = do
  pos <- alienPos <+= V2 0 (-0.01)
  when (pos ^. _y < -1.5) $
    alienDead .= True

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
  playerVel %= (^* 0.6)
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
  charge <- playerShootCharge <+= 0.05
  when (input ^. shootPressed && charge >= 1) $ do
    playerShootCharge .= 0
    use playerPos >>= (tell . (:[]) . newBullet)

newBullet :: Vec -> Bullet
newBullet pos = Bullet
  { _bulletPos = pos
  , _bulletDead = False
  }

tickBullet :: State Bullet ()
tickBullet = do
  pos <- bulletPos <+= (V2 0 0.03)
  when (pos ^. _y > 1.5) $
    bulletDead .= True

letBulletsHit :: State Game ()
letBulletsHit = do
  oldAliens <- use aliens
  oldBullets <- use bullets
  let (newAliens, newBullets) =
        mapPairs alienBulletCollision oldAliens oldBullets
  aliens .= newAliens
  bullets .= newBullets

alienBulletCollision :: Alien -> Bullet -> (Alien, Bullet)
alienBulletCollision a b =
  if isCollision a b
  then (a & alienDead .~ True, b & bulletDead .~ True)
  else (a, b)

class Collidable a where
  center :: a -> Vec
  radius :: a -> Float

instance Collidable Alien where
  center a = a ^. alienPos
  radius a = 0.05

instance Collidable Bullet where
  center b = b ^. bulletPos
  radius b = 0.05

isCollision :: (Collidable a, Collidable b) => a -> b -> Bool
isCollision a b = norm (center a - center b) < radius a + radius b

mapPairs :: (a -> b -> (a, b)) -> [a] -> [b] -> ([a], [b])
mapPairs modification [] bs = ([], bs)
mapPairs modification (a:as) bs = (a':as', bs'')
  where
    (a', bs') = inner a bs
    (as', bs'') = mapPairs modification as bs'
    inner a [] = (a, [])
    inner a (b:bs) =
      let
        (a', b') = modification a b
        (a'', bs') = inner a' bs
      in (a'', b':bs')

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
