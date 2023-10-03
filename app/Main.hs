{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent
import Data.Function (fix)
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC

wWidth, wHeight :: Num a => a
wWidth = cSize * bWidth
wHeight = cSize * (bHeight - 1)

bWidth, bHeight :: Num a => a
bWidth = 6
bHeight = 14

puyoColor :: Char -> Color
puyoColor 'G' = green
puyoColor 'R' = red
puyoColor 'Y' = yellow
puyoColor 'B' = blue
puyoColor 'P' = violet
puyoColor ' ' = white
puyoColor 'O' = greyN 0.6

window :: Display
window = InWindow "PuyoPuyo" (wWidth, wHeight) (100, 100)

main :: IO ()
main = do
	world <- generateNewWorld 
	playIO window white 10 world drawWorld eventHandler stepWorld

cSize, cWidth, cHeight :: Num a => a
cSize = 50
cWidth = fromIntegral $ wWidth `div` cSize
cHeight = fromIntegral $ wHeight `div` cSize

type Position = (Int, Int) 			--(x, y)
type PuyoPos = (Position, Char)

randomPosition :: GenIO -> IO Position
randomPosition gen = (,) <$> uniformR (0, cWidth - 1) gen <*> uniformR (0, cHeight - 1) gen

isTherePuyo :: [PuyoPos] -> Position -> Bool
isTherePuyo b p = p `elem` map fst b

data HandAction = SAStop | SADown | SALeft | SARight deriving Eq

moveHand :: HandAction -> Position -> Position
moveHand SAStop (x, y) = (x, y - 1)
moveHand SADown (x, y) = (x, y - 1)
moveHand SALeft (x, y) = (x - 1, y - 1)
moveHand SARight (x, y) = (x + 1, y - 1)

data GameState = InGame | PuttingPuyo | GameOver

data World = World
	{ _state :: GameState
	, _snake :: [Position]
	, _action :: HandAction
	, _score :: Int
	, _board :: [PuyoPos]
	, _deathblock :: Position
	}

generateNewWorld :: IO World
generateNewWorld = do
	snakeH <- withSystemRandom . asGenIO $ \gen -> do
		fix $ \loop -> do
			snakeH <- randomPosition gen
			if snakeH == ((-10), (-10)) then loop else pure snakeH
	let board_1 = ((3, 5), 'R')
	let board_2 = ((4, 5), 'G')
	let board_3 = ((2, 5), 'P')
	let board_4 = ((1, 5), 'O')
	let board_5 = ((1, 3), 'B')
	let board_6 = ((1, 2), 'Y')
	pure $ World InGame [(2, 12)] SAStop 0 [board_1, board_2, board_3, board_4, board_5, board_6] (2, 12)

drawWorld :: World -> IO Picture
drawWorld World{..} = case _state of
	InGame -> pure $ pictures
		[ drawCell (greyN 0.3) (head _snake)
		, drawCell black _deathblock
		, pictures $ map (drawCell (greyN 0.6)) (tail _snake)
		, translate (-wWidth/2+10) (-wHeight/2+10) . scale 0.2 0.2 $ text ("SCORE: " ++ show _score)
		, pictures (map (\(pos, char) -> drawCell (puyoColor char) pos) _board)
		]
		where
			cell = translate (-wWidth/2) (-wHeight/2) $ polygon [(0, 0), (0, cSize), (cSize, cSize), (cSize, 0)]
			drawCell c (x, y) = translate (fromIntegral x * cSize) (fromIntegral y * cSize) $ color c cell
	GameOver -> pure $ pictures
		[ translate (-270) 20   . scale 0.7 0.7 $ text "Game Over"
		, translate (-100) (-50) . scale 0.3 0.3 $ text ("Score: " ++ show _score)
		, translate (-200) (-120) . scale 0.3 0.3 $ text "Press Enter to Retry"
		]

eventHandler :: Event -> World -> IO World
eventHandler e w@World{..} = case _state of
	InGame -> case e of
		EventKey (SpecialKey KeyDown) Down _ _ -> pure $ w { _action = SADown }
		EventKey (SpecialKey KeyLeft) Down _ _ -> pure $ w { _action = SALeft }
		EventKey (SpecialKey KeyRight) Down _ _ -> pure $ w { _action = SARight }
		_ -> pure w
	GameOver -> case e of
		EventKey (SpecialKey KeyEnter) Down _ _ -> generateNewWorld
		_ -> pure w

stepWorld :: Float -> World -> IO World
stepWorld _ w@World{..} = case _state of
	InGame -> do
		let (x, y) = moveHand _action $ head _snake
		let snake = (x, y) : _snake
		if isTherePuyo _board _deathblock == True
			then pure $ w {_state = GameOver}
			else pure $ w {_snake = init snake}
		if isTherePuyo _board (x, y) == True || y < 0
			then pure $ w {_state = PuttingPuyo, _snake = init snake}
			else pure $ w {_snake = init snake}
		if (x >= cWidth && _action == SARight) || (x < 0 && _action == SALeft)
			then pure $ w {_action = SAStop, _snake = init snake}
			else pure $ w {_snake = init snake}
	PuttingPuyo -> do
		putStrLn "Putting Puyo!!"
		pure $ w {_state = InGame, _snake = [(2, 12)], _action = SAStop}
	GameOver -> pure w




