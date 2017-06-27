{- 

worms - a very simple FunGEn example.
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

data GameAttribute = GA Int Bool (GLdouble,GLdouble) Int
data ObjectAttribute = NoObjectAttribute | Tail Int
data GameState = LevelStart Int | Level Int | GameOver
data TileAttribute = NoTileAttribute

type WormsAction a = IOGame GameAttribute ObjectAttribute GameState TileAttribute a
type WormsObject = GameObject ObjectAttribute
type WormsTile = Tile TileAttribute
type WormsMap = TileMatrix TileAttribute

tileSize, speedMod :: GLdouble
tileSize = 30.0
speedMod = 30.0

initPos :: (GLdouble,GLdouble)
initPos  = (45.0,105.0)
--GOL OBJETIVO
objetivo :: Bool
objetivo = False

maxFood, initTailSize, defaultTimer :: Int
maxFood = 10
initTailSize = 2
defaultTimer = 10

white :: InvList
white = Just[(255,255,255)]

bmpList :: FilePictureList
bmpList = [("level1.bmp",          Nothing),
           ("level2.bmp",          Nothing),
           ("level3.bmp",          Nothing),
           ("gameover.bmp",        Nothing),
           ("congratulations.bmp", Nothing),
           ("bola.bmp",           white),
           ("bola.bmp",           white),
           ("bola.bmp",           white),
           ("bola.bmp",           white),
           ("food.bmp",            Nothing),
           ("segment.bmp",         Nothing),
           ("border1.bmp",         Nothing),
           ("border2.bmp",         Nothing),
           ("border3.bmp",         Nothing),
           ("free1.bmp",           Nothing),
           ("free2.bmp",           Nothing),
           ("free3.bmp",           Nothing),
           ("spike.bmp",           Nothing),
           ("elephant.bmp",        Nothing)]

-- position of the paths in the list:
border1, border2, border3, free1, bl, br, bu, bd,spike,elephant :: Int
border1 = 11
border2 = 12
border3 = 13
free1   = 14
bl      = 15
br      = 16
bu      = 10
bd      = 12
spike   = 17
elephant = 18 

main :: IO ()
main = do
  let winConfig = ((200,100),(780,600),"PLC - GAME")

      gameMap = multiMap [(tileMap map1 tileSize tileSize),
                          (tileMap map2 tileSize tileSize),
                          (tileMap map3 tileSize tileSize)] 0

      gameAttribute = GA defaultTimer objetivo initPos 0

      groups = [(objectGroup "messages"  createMsgs ),
                (objectGroup "head"     [createHead]),
                (objectGroup "food"     [createFood])]

      input = [
               (SpecialKey KeyLeft,  Press, turnLeft ),
               (SpecialKey KeyRight, Press, turnRight),
               (SpecialKey KeyUp,    Press, turnUp   ),
               (SpecialKey KeyDown,  Press, turnDown )
              ,(Char 'q',            Press, \_ _ -> funExit)
              ]
  
  bmpList' <- mapM (\(a,b) -> do { a' <- getDataFileName ("examples/jgm/"++a); return (a', b)}) bmpList
  funInit winConfig gameMap groups (LevelStart 1) gameAttribute input gameCycle (Timer 150) bmpList'

createMsgs :: [WormsObject]
createMsgs =
  let picLevel1          = Tex (150,50)  0
      picLevel2          = Tex (150,50)  1
      picLevel3          = Tex (150,50)  2
      picGameOver        = Tex (300,100) 3
      picCongratulations = Tex (300,100) 4
  in [(object "level1"          picLevel1          True (395,300) (0,0) NoObjectAttribute),
      (object "level2"          picLevel2          True (395,300) (0,0) NoObjectAttribute),
      (object "level3"          picLevel3          True (395,300) (0,0) NoObjectAttribute),
      (object "gameover"        picGameOver        True (395,300) (0,0) NoObjectAttribute),
      (object "congratulations" picCongratulations True (395,300) (0,0) NoObjectAttribute)]

createHead :: WormsObject
createHead = let pic = Tex (tileSize,tileSize) 5
             in object "head" pic True initPos (0,speedMod) NoObjectAttribute

createFood :: WormsObject
createFood = let pic = Tex (tileSize,tileSize) 9
             in object "food" pic True (0,0) (0,0) NoObjectAttribute 


turnLeft :: Modifiers -> Position -> WormsAction ()
turnLeft _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture 8 snakeHead
  headPos <- getObjectPosition snakeHead
  tile <- getTileFromWindowPosition headPos
  if ((getTilePictureIndex tile) == 15)
	then do stop
	else do setObjectSpeed (-speedMod,0) snakeHead
    
turnRight :: Modifiers -> Position -> WormsAction ()
turnRight _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture 7 snakeHead
  headPos <- getObjectPosition snakeHead
  tile <- getTileFromWindowPosition headPos
  if ((getTilePictureIndex tile) == 16)
	then do stop
	else do setObjectSpeed (speedMod,0) snakeHead

turnUp :: Modifiers -> Position -> WormsAction ()
turnUp _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture 5 snakeHead
  headPos <- getObjectPosition snakeHead
  tile <- getTileFromWindowPosition headPos
  if ((getTilePictureIndex tile) == 10)
	then do stop
	else do setObjectSpeed (0,speedMod) snakeHead

turnDown :: Modifiers -> Position -> WormsAction ()
turnDown _ _ = do
  snakeHead <- findObject "head" "head"
  setObjectCurrentPicture 6 snakeHead
  headPos <- getObjectPosition snakeHead
  tile <- getTileFromWindowPosition headPos
  if ((getTilePictureIndex tile) == 12)
	then do stop
	else do setObjectSpeed (0,-speedMod) snakeHead

stop :: WormsAction ()
stop = do
  snakeHead <- findObject "head" "head"
  setObjectSpeed (0,0) snakeHead


gameCycle :: WormsAction ()
gameCycle = do
  (GA timer objetivo previousHeadPos score) <- getGameAttribute
  gState <- getGameState
  case gState of
      LevelStart n -> case n of
                        4 -> do
                              congratulations <- findObject "congratulations" "messages"
                              drawObject congratulations
                              if (timer == 0)
                                  then funExit
                                  else (setGameAttribute (GA (timer - 1) objetivo previousHeadPos score))
                        _ -> do
                              disableGameFlags
                              level <- findObject ("level" ++ (show n)) "messages"
                              drawObject level
                              if (timer == 0)
                                  then (do setGameState (Level n)
                                           enableGameFlags
                                           snakeHead <- findObject "head" "head"
                                           setObjectAsleep False snakeHead
                                           setObjectPosition initPos snakeHead
                                           setObjectSpeed (0.0,speedMod) snakeHead
                                           setObjectCurrentPicture 5 snakeHead
                                           setGameAttribute (GA defaultTimer objetivo previousHeadPos score)
                                           destroyObject level
                                           setNewMap n)
                                  else setGameAttribute (GA (timer - 1) objetivo previousHeadPos score)
      Level n -> do
                  snakeHead <- findObject "head" "head"
                  if (objetivo==True) -- advance level!
                      then  (do setGameState (LevelStart (n + 1))
                                disableGameFlags
                                setGameAttribute (GA timer False initPos score)
				)
                                else (do checkSnakeCollision snakeHead
					 headPos <- getObjectPosition snakeHead
 					 tile <- getTileFromWindowPosition headPos
 					 if((getTilePictureIndex tile)==18) 
                                           then setGameAttribute(GA timer True initPos score)
				     	   else return () 
				)
                 
  {-                    else if (timer == 0) -- put a new food in the map
                             then (do food <- findObject "food" "food"
                                      newPos <- createNewFoodPosition
                                      setObjectPosition newPos food
                                      newFood <- findObject "food" "food"
                                      setObjectAsleep False newFood
                                      setGameAttribute (GA (-1) remainingFood tailSize previousHeadPos score)
                                      snakeHead <- findObject "head" "head"
                                      checkSnakeCollision snakeHead )
                             else if (timer > 0) -- there is no food in the map, so decrease the food timer
                                   then (do setGameAttribute (GA (timer - 1) remainingFood tailSize previousHeadPos score)
                                            snakeHead <- findObject "head" "head"
                                            checkSnakeCollision snakeHead )
                                   else (do -- there is a food in the map
                                      food <- findObject "food" "food"
                                      snakeHead <- findObject "head" "head"
                                      col <- objectsCollision snakeHead food
                                      if col
                                          then (do snakeHeadPosition <- getObjectPosition snakeHead
                                                   setGameAttribute (GA defaultTimer (remainingFood-1) (tailSize + 1) snakeHeadPosition (score + 1))
                                                   --addTail previousHeadPos
                                                   setObjectAsleep True food)
                                          else checkSnakeCollision snakeHead) 
                  showScore -}

      GameOver -> do
                      disableMapDrawing
                      gameover <- findObject "gameover" "messages"
                      drawMap
                      drawObject gameover
                      if (timer == 0)
                              then funExit
                              else (setGameAttribute (GA (timer - 1) objetivo (0,0) 0))


{-
showScore :: WormsAction ()
showScore = do
  (GA _ remainingFood _ _ score) <- getGameAttribute
  printOnScreen (printf "Score: %d    Food remaining: %d" score remainingFood) TimesRoman24 (40,8) 1.0 1.0 1.0
  showFPS TimesRoman24 (780-60,8) 1.0 0.0 0.0
-}
setNewMap :: Int -> WormsAction ()
setNewMap 2 = setCurrentMapIndex 1
setNewMap 3 = setCurrentMapIndex 2
setNewMap _ = return ()

--colisao com espinho é morte
--colisão com objeto para , determinando chão
checkSnakeCollision :: WormsObject -> WormsAction ()
checkSnakeCollision snakeHead = do
  headPos <- getObjectPosition snakeHead
  tile <- getTileFromWindowPosition headPos
  --tails <- getObjectsFromGroup "tail"
  --col <- objectListObjectCollision tails snakeHead
  if ((getTileBlocked tile))
          then (do setGameState GameOver
                   disableObjectsDrawing
                   disableObjectsMoving
                   setGameAttribute (GA defaultTimer objetivo (0,0) 0))
          else if (((getTilePictureIndex tile)==11) || ((getTilePictureIndex tile) == 15) || ((getTilePictureIndex tile) == 16) ||        ((getTilePictureIndex tile) == 10) || ((getTilePictureIndex tile) == 12))
          then stop
          else return()

createNewFoodPosition :: WormsAction (GLdouble,GLdouble)
createNewFoodPosition = do
  x <- randomInt (1,18)
  y <- randomInt (1,24)
  mapPositionOk <- checkMapPosition (x,y)
  if (mapPositionOk)
      then (return (toPixelCoord y,toPixelCoord x))
      else createNewFoodPosition
  where toPixelCoord a = (tileSize/2) + (fromIntegral a) * tileSize

checkMapPosition :: (Int,Int) -> WormsAction Bool
checkMapPosition (x,y) = do
  mapTile <- getTileFromIndex (x,y)
  return (not (getTileBlocked mapTile))

b,f,s,x,y,w,z,gol :: WormsTile
b = (border1, False,  0.0, NoTileAttribute) --cost 1 chão
f = (free1,   False, 0.0, NoTileAttribute)
s = (spike,True,0.0,NoTileAttribute)
x= (bl,False,0.0,NoTileAttribute)
y= (br,False,0.0,NoTileAttribute)
w= (bu,False,0.0,NoTileAttribute)
z= (bd,False,0.0,NoTileAttribute)
gol = (elephant,False,0.0,NoTileAttribute)
map1 :: WormsMap
map1 = [[b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,gol,y],
        [b,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,b]]

map2 :: WormsMap
map2 = [[b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,gol,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [b,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,b]]

map3 :: WormsMap
map3 = [[b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,gol,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [b,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,b]]

