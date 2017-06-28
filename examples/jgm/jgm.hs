{- 
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>
-}

module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

data GameAttribute = GA Int Bool (GLdouble,GLdouble) Int
data ObjectAttribute = NoObjectAttribute 
data GameState = LevelStart Int | Level Int | GameOver
data TileAttribute = NoTileAttribute

type JGMAction a = IOGame GameAttribute ObjectAttribute GameState TileAttribute a
type JGMObject = GameObject ObjectAttribute
type JGMTile = Tile TileAttribute
type JGMMap = TileMatrix TileAttribute


--Tamanho do quadro e velocidade
tileSize, speedMod :: GLdouble
tileSize = 30.0
speedMod = 30.0

--Posição Inicial Bola
initPos = (40.0,550.0) :: (GLdouble,GLdouble)


--GOL OBJETIVO
objetivo :: Bool
objetivo = False

defaultTimer :: Int
defaultTimer = 10

white,black :: InvList
white = Just[(255,255,255)]
black = Just[(0,0,0)]

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
           ("grass.bmp",           Nothing),
           ("free2.bmp",           Nothing),
           ("free3.bmp",           Nothing),
           ("spike.bmp",           white),
           ("barra.bmp",      Nothing),
           ("luva.bmp",black)]

-- position of the paths in the list:
border1, border2, border3, grass, bl, br, bu, bd,spike,barra :: Int
border1 = 11
border2 = 12
border3 = 13
grass   = 14
bl      = 15
br      = 16
bu      = 10
bd      = 12
spike   = 17
barra = 18 


main :: IO ()
main = do
  let winConfig = ((200,100),(1050,600),"PLC - GAME - JGM")

      gameMap = multiMap [(tileMap map1 tileSize tileSize),
                          (tileMap map2 tileSize tileSize),
                          (tileMap map3 tileSize tileSize)] 0

      gameAttribute = GA defaultTimer objetivo initPos 0

      groups = [(objectGroup "messages"  createMsgs ),
                (objectGroup "bola"     [createBola]),
                (objectGroup "luvas"     createLuvas)]
--ADICIONAR BOTÃO PARAR???
      input = [
               (SpecialKey KeyLeft,  Press, turnLeft ),
               (SpecialKey KeyRight, Press, turnRight),
               (SpecialKey KeyUp,    Press, turnUp   ),
               (SpecialKey KeyDown,  Press, turnDown )
              ,(Char 'q',            Press, \_ _ -> funExit)
              ]
  
  bmpList' <- mapM (\(a,b) -> do { a' <- getDataFileName ("examples/jgm/"++a); return (a', b)}) bmpList
  funInit winConfig gameMap groups (LevelStart 1) gameAttribute input gameCycle (Timer 150) bmpList'

createMsgs :: [JGMObject]
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

createBola :: JGMObject
createBola = let pic = Tex (tileSize,tileSize) 5
             in object "bola" pic True initPos (0,speedMod) NoObjectAttribute

createLuvas :: [JGMObject]
createLuvas = let pic = Tex (tileSize*1.5,tileSize*1.5) 19
           in[(object "luva1" pic True (45.0,405.0) (50,0) NoObjectAttribute),
              (object "luva2" pic True (45.0,205.0) (50,0) NoObjectAttribute),
              (object "luva3" pic True (45.0,105.0) (50,0) NoObjectAttribute),
	      (object "luva4" pic True (305.0,555.0) (50,0) NoObjectAttribute),
              (object "luva5" pic True (45.0,305.0) (50,0) NoObjectAttribute),
	      (object "luva6" pic True (205.0,305.0) (0,50) NoObjectAttribute),
	      (object "luva7" pic True (405.0,550.0) (0,50) NoObjectAttribute)] 

moveLuva:: JGMObject -> JGMAction()
moveLuva luva = do 
              luvaPos <- getObjectPosition luva
	      col1 <- objectLeftMapFutureCollision luva
              col2 <- objectRightMapFutureCollision luva
              when (col1 || col2) (reverseXSpeed luva)
     
moveLuvaVertical :: JGMObject -> JGMAction()
moveLuvaVertical luva = do
	luvaPos <- getObjectPosition luva
	col1 <- objectTopMapCollision luva
        col2 <- objectBottomMapCollision luva
	when (col1 || col2) (reverseYSpeed luva)  	

turnLeft :: Modifiers -> Position -> JGMAction ()
turnLeft _ _ = do
  bola <- findObject "bola" "bola"
  setObjectCurrentPicture 8 bola
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  if ((getTilePictureIndex tile) == 15)
	then do stop
	else do setObjectSpeed (-speedMod,0) bola
    
turnRight :: Modifiers -> Position -> JGMAction ()
turnRight _ _ = do
  bola <- findObject "bola" "bola"
  setObjectCurrentPicture 7 bola
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  if ((getTilePictureIndex tile) == 16)
	then do stop
	else do setObjectSpeed (speedMod,0) bola

turnUp :: Modifiers -> Position -> JGMAction ()
turnUp _ _ = do
  bola <- findObject "bola" "bola"
  setObjectCurrentPicture 5 bola
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  if ((getTilePictureIndex tile) == 10)
	then do stop
	else do setObjectSpeed (0,speedMod) bola

turnDown :: Modifiers -> Position -> JGMAction ()
turnDown _ _ = do
  bola <- findObject "bola" "bola"
  setObjectCurrentPicture 6 bola
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  if ((getTilePictureIndex tile) == 12)
	then do stop
	else do setObjectSpeed (0,-speedMod) bola

stop :: JGMAction ()
stop = do
  bola <- findObject "bola" "bola"
  setObjectSpeed (0,0) bola


gameCycle :: JGMAction ()
gameCycle = do
  (GA timer objetivo previousbolaPos score) <- getGameAttribute
  gState <- getGameState
  case gState of
      LevelStart n -> case n of
                        4 -> do
                              congratulations <- findObject "congratulations" "messages"
                              drawObject congratulations
                              if (timer == 0)
                                  then funExit
                                  else (setGameAttribute (GA (timer - 1) objetivo previousbolaPos score))
                        n -> do
                              disableGameFlags
                              level <- findObject ("level" ++ (show n)) "messages"
                              drawObject level
                              if (timer == 0)
                                  then (do setGameState (Level n)
                                           enableGameFlags
                                           bola <- findObject "bola" "bola"
                                           setObjectAsleep False bola
                                           setObjectPosition initPos bola
                                           setObjectSpeed (0.0,0.0) bola
                                           setObjectCurrentPicture 5 bola 
                                           setGameAttribute (GA defaultTimer objetivo previousbolaPos score)
                                           destroyObject level
                                           setNewMap n)
                                  else setGameAttribute (GA (timer - 1) objetivo previousbolaPos score)
			  
      Level n -> do
                  bola <- findObject "bola" "bola"
                  if (objetivo==True) -- advance level!
                      then  (do setGameState (LevelStart (n + 1))
                                disableGameFlags
                                setGameAttribute (GA timer False initPos score)                                   
				)
                                else (do checkCollision bola
                                         when (n==1) (do
                                           luvaEnemy1 <- findObject "luva1" "luvas"
                                           moveLuva luvaEnemy1  
                                           luvaEnemy2 <- findObject "luva2" "luvas" 
                                           moveLuva luvaEnemy2  
                                      	   luvaEnemy3 <- findObject "luva3" "luvas" 
                                           moveLuva luvaEnemy3
                                           setObjectAsleep False luvaEnemy1    
                                           setObjectAsleep False luvaEnemy2    
				           setObjectAsleep False luvaEnemy3 
                                          )                                          
                                         when(n==2) (do
                                          luvaEnemy1 <- findObject "luva1" "luvas"
                                          moveLuva luvaEnemy1
				          luvaEnemy2 <- findObject "luva2" "luvas" 
                                          moveLuva luvaEnemy2  
                                      	  luvaEnemy3 <- findObject "luva3" "luvas" 
                                          moveLuva luvaEnemy3 
					  luvaEnemy4 <- findObject "luva4" "luvas"	
                                          moveLuva luvaEnemy4
                                          luvaEnemy6 <- findObject "luva6" "luvas" 
                                          moveLuvaVertical luvaEnemy6 
                                          setObjectAsleep False luvaEnemy1    
                                          setObjectAsleep False luvaEnemy2    
				          setObjectAsleep False luvaEnemy3
                                          setObjectAsleep False luvaEnemy4
                                          setObjectAsleep False luvaEnemy6       		                                  
                                          )
                                         when(n==3)(do
					  luvaEnemy1 <- findObject "luva1" "luvas"
                                          moveLuva luvaEnemy1
				          luvaEnemy2 <- findObject "luva2" "luvas" 
                                          moveLuva luvaEnemy2  
                                      	  luvaEnemy3 <- findObject "luva3" "luvas" 
                                          moveLuva luvaEnemy3
                                          luvaEnemy4 <- findObject "luva4" "luvas" 
                                          moveLuva luvaEnemy4
                                          luvaEnemy5 <- findObject "luva5" "luvas" 
                                          moveLuva luvaEnemy5
                                          luvaEnemy6 <- findObject "luva6" "luvas" 
                                          moveLuvaVertical luvaEnemy6
                                          luvaEnemy7 <- findObject "luva7" "luvas" 
                                          moveLuvaVertical luvaEnemy7                                         
                                          setObjectAsleep False luvaEnemy1    
                                          setObjectAsleep False luvaEnemy2    
				          setObjectAsleep False luvaEnemy3
                                          setObjectAsleep False luvaEnemy4    
				          setObjectAsleep False luvaEnemy5
                                          setObjectAsleep False luvaEnemy6    
				          setObjectAsleep False luvaEnemy7  
					  ) 	  						
					 bolaPos <- getObjectPosition bola
 					 tile <- getTileFromWindowPosition bolaPos
 					 if((getTilePictureIndex tile)==18) 
                                           then setGameAttribute(GA timer True initPos score)
				     	   else return ())
                                           
                 


      GameOver -> do
                      disableMapDrawing
                      gameover <- findObject "gameover" "messages"
                      drawMap
                      drawObject gameover
                      if (timer == 0)
                              then funExit
                              else (setGameAttribute (GA (timer - 1) objetivo (0,0) 0))


{-
showScore :: JGMAction ()
showScore = do
  (GA _ remainingFood _ _ score) <- getGameAttribute
  printOnScreen (printf "Score: %d    Food remaining: %d" score remainingFood) TimesRoman24 (40,8) 1.0 1.0 1.0
  showFPS TimesRoman24 (780-60,8) 1.0 0.0 0.0
-}
setNewMap :: Int -> JGMAction ()
setNewMap 2 = setCurrentMapIndex 1
setNewMap 3 = setCurrentMapIndex 2
setNewMap _ = return ()
 --tails <- getObjectsFromGroup "tail"
--colisao com espinho é morte
--colisão com objeto para , determinando chão
checkCollision :: JGMObject -> JGMAction ()
checkCollision bola = do
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  luvas <- getObjectsFromGroup "luvas"
  col <- objectListObjectCollision luvas bola
  if ((getTileBlocked tile || col ))
          then (do setGameState GameOver
                   disableObjectsDrawing
                   disableObjectsMoving
                   setGameAttribute (GA defaultTimer objetivo (0,0) 0))
          else if (((getTilePictureIndex tile)==11) || ((getTilePictureIndex tile) == 15) || ((getTilePictureIndex tile) == 16) ||        ((getTilePictureIndex tile) == 10) || ((getTilePictureIndex tile) == 12))
          then stop
          else return()

{-
createNewFoodPosition :: JGMAction (GLdouble,GLdouble)
createNewFoodPosition = do
  x <- randomInt (1,18)
  y <- randomInt (1,24)
  mapPositionOk <- checkMapPosition (x,y)
  if (mapPositionOk)
      then (return (toPixelCoord y,toPixelCoord x))
      else createNewFoodPosition
  where toPixelCoord a = (tileSize/2) + (fromIntegral a) * tileSize

checkMapPosition :: (Int,Int) -> JGMAction Bool
checkMapPosition (x,y) = do
  mapTile <- getTileFromIndex (x,y)
  return (not (getTileBlocked mapTile))
-}
b,f,s,x,y,w,z,gol :: JGMTile
b = (border1, False,  0.0, NoTileAttribute) --cost 1 chão
f = (grass,   False, 0.0, NoTileAttribute)
s = (spike,True,0.0,NoTileAttribute)
x= (bl,False,0.0,NoTileAttribute)
y= (br,False,0.0,NoTileAttribute)
w= (bu,False,0.0,NoTileAttribute)
z= (bd,False,0.0,NoTileAttribute)
gol = (barra,False,0.0,NoTileAttribute)


map1 :: JGMMap
map1 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,gol,y],
        [s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]

map2 :: JGMMap
map2 = [[b,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,b],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,y],
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

map3 :: JGMMap
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
        [x,f,f,f,f,f,f,f,f,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,gol,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [b,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,b]]

