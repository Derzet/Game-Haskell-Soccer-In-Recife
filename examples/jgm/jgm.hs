
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
speedMod = 40.0

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
           ("campeao.bmp", Nothing),
           ("bola.bmp",           white),
           ("bola.bmp",           white),
           ("bola.bmp",           white),
           ("bola.bmp",           white),
           ("food.bmp",            Nothing),
           ("bordaHTop.bmp",       Nothing),
           ("bordaHDown.bmp",         Nothing),
           ("bordaVLeft.bmp",         Nothing),
           ("bordaVRight.bmp",         Nothing),
           ("grass.bmp",           Nothing),
           ("spike.bmp",           white),
           ("barra.bmp",      Nothing),
           ("luva.bmp",black),
           ("lama.bmp",Nothing),
           ("aflitos.bmp", Nothing),
           ("ilha.bmp", Nothing),        
           ("arruda.bmp", Nothing),
           ("arena.bmp", Nothing),
           ("salgueiro.bmp", Nothing)]
-- position of the paths in the list:
grass, bl, br, bu, bd,spike,barra,lama :: Int
bu = 10
bd = 11
bl = 12
br = 13
grass  = 14
spike  = 15
barra = 16 
lama = 18


main :: IO ()
main = do
  let winConfig = ((200,100),(1050,600),"PLC - GAME - JGM")

      gameMap = multiMap [(tileMap map1 tileSize tileSize),
                          (tileMap map2 tileSize tileSize),
                          (tileMap map3 tileSize tileSize),
                          (tileMap map4 tileSize tileSize),
                          (tileMap map5 tileSize tileSize)] 0

      gameAttribute = GA defaultTimer objetivo initPos 0

      groups = [(objectGroup "messages"  createMsgs ),
                (objectGroup "bola"     [createBola]),
                (objectGroup "inimigos"     createInimigos)]
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
  let gameOver        = Tex (400,200) 3
      campeao = Tex (718,519) 4
      aflitos = Tex(654,500) 19
      ilha = Tex(654,500) 20
      arruda = Tex(654,500) 21
      arena = Tex(654,500) 22
      salgueiro = Tex(654,500) 23      		          	
  in [(object "level1"          arena          True (555,300) (0,0) NoObjectAttribute),
      (object "level2"          salgueiro          True (555,300) (0,0) NoObjectAttribute),
      (object "level3"          aflitos          True (555,300) (0,0) NoObjectAttribute),
      (object "level4"          arruda          True (555,300) (0,0) NoObjectAttribute),
      (object "level5"          ilha          True (555,300) (0,0) NoObjectAttribute),
      (object "gameover"        gameOver        True (555,300) (0,0) NoObjectAttribute),
      (object "campeao" campeao True (555,300) (0,0) NoObjectAttribute)
      ]	

createBola :: JGMObject
createBola = let pic = Tex (tileSize,tileSize) 5
             in object "bola" pic True initPos (0,speedMod) NoObjectAttribute

createInimigos :: [JGMObject]
createInimigos = let pic = Tex (tileSize*1.5,tileSize*1.5) 17
           in[(object "luva1" pic True (45.0,405.0) (30,0) NoObjectAttribute),
              (object "luva2" pic True (45.0,205.0) (30,0) NoObjectAttribute),
              (object "luva3" pic True (45.0,105.0) (30,0) NoObjectAttribute),
	      (object "luva4" pic True (305.0,555.0) (30,0) NoObjectAttribute),
              (object "luva5" pic True (45.0,305.0) (30,0) NoObjectAttribute),
	      (object "luva6" pic True (205.0,305.0) (0,30) NoObjectAttribute),
	      (object "luva7" pic True (405.0,550.0) (0,30) NoObjectAttribute)] 

moveLuva:: JGMObject -> JGMAction()
moveLuva luva = do 
              --luvaPos <- getObjectPosition luva
	      col1 <- objectLeftMapFutureCollision luva
              col2 <- objectRightMapFutureCollision luva
              when (col1 || col2) (reverseXSpeed luva)
     
moveLuvaVertical :: JGMObject -> JGMAction()
moveLuvaVertical luva = do
	--luvaPos <- getObjectPosition luva
	col1 <- objectTopMapCollision luva
        col2 <- objectBottomMapCollision luva
	when (col1 || col2) (reverseYSpeed luva)  	

turnLeft :: Modifiers -> Position -> JGMAction ()
turnLeft _ _ = do
  bola <- findObject "bola" "bola"
  setObjectCurrentPicture 8 bola
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  if ((getTilePictureIndex tile) == 12)
	then do stop
	else do setObjectSpeed (-speedMod,0) bola
    
turnRight :: Modifiers -> Position -> JGMAction ()
turnRight _ _ = do
  bola <- findObject "bola" "bola"
  setObjectCurrentPicture 7 bola
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  if ((getTilePictureIndex tile) == 13)
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
  if ((getTilePictureIndex tile) == 11)
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
                        6 -> do
                              campeao <- findObject "campeao" "messages"
                              drawObject campeao
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
                                           luvaEnemy1 <- findObject "luva1" "inimigos"
                                           moveLuva luvaEnemy1  
                                           luvaEnemy2 <- findObject "luva2" "inimigos" 
                                           moveLuva luvaEnemy2  
                                      	   luvaEnemy3 <- findObject "luva3" "inimigos" 
                                           moveLuva luvaEnemy3
                                           setObjectAsleep False luvaEnemy1    
                                           setObjectAsleep False luvaEnemy2    
				           setObjectAsleep False luvaEnemy3 
                                          )                                          
                                         when(n==2) (do
                                          luvaEnemy1 <- findObject "luva1" "inimigos"
                                          moveLuva luvaEnemy1
				          luvaEnemy2 <- findObject "luva2" "inimigos" 
                                          moveLuva luvaEnemy2  
                                      	  luvaEnemy3 <- findObject "luva3" "inimigos" 
                                          moveLuva luvaEnemy3 
					  luvaEnemy4 <- findObject "luva4" "inimigos"	
                                          moveLuva luvaEnemy4
                                          luvaEnemy6 <- findObject "luva6" "inimigos" 
                                          moveLuvaVertical luvaEnemy6 
                                          setObjectAsleep False luvaEnemy1    
                                          setObjectAsleep False luvaEnemy2    
				          setObjectAsleep False luvaEnemy3
                                          setObjectAsleep False luvaEnemy4
                                          setObjectAsleep False luvaEnemy6       		                                  
                                          )
                                         when(n==3)(do
					  luvaEnemy1 <- findObject "luva1" "inimigos"
                                          moveLuva luvaEnemy1
				          luvaEnemy2 <- findObject "luva2" "inimigos" 
                                          moveLuva luvaEnemy2  
                                      	  luvaEnemy3 <- findObject "luva3" "inimigos" 
                                          moveLuva luvaEnemy3
                                          luvaEnemy4 <- findObject "luva4" "inimigos" 
                                          moveLuva luvaEnemy4
                                          luvaEnemy5 <- findObject "luva5" "inimigos" 
                                          moveLuva luvaEnemy5
                                          luvaEnemy6 <- findObject "luva6" "inimigos" 
                                          moveLuvaVertical luvaEnemy6
                                                                               
                                          setObjectAsleep False luvaEnemy1    
                                          setObjectAsleep False luvaEnemy2    
				          setObjectAsleep False luvaEnemy3
                                          setObjectAsleep False luvaEnemy4    
				          setObjectAsleep False luvaEnemy5
                                          setObjectAsleep False luvaEnemy6    
				        
					  )
					 when (n==4) (do
                                          luvaEnemy1 <- findObject "luva1" "inimigos"
                                          moveLuva luvaEnemy1
				          luvaEnemy2 <- findObject "luva2" "inimigos" 
                                          moveLuva luvaEnemy2  
                                      	  luvaEnemy3 <- findObject "luva3" "inimigos" 
                                          moveLuva luvaEnemy3
                                          luvaEnemy4 <- findObject "luva4" "inimigos" 
                                          moveLuva luvaEnemy4
                                          luvaEnemy5 <- findObject "luva5" "inimigos" 
                                          moveLuva luvaEnemy5
                                          luvaEnemy6 <- findObject "luva6" "inimigos" 
                                          moveLuvaVertical luvaEnemy6
                                                                               
                                          setObjectAsleep False luvaEnemy1    
                                          setObjectAsleep False luvaEnemy2    
				          setObjectAsleep False luvaEnemy3
                                          setObjectAsleep False luvaEnemy4    
				          setObjectAsleep False luvaEnemy5
                                          setObjectAsleep False luvaEnemy6    
                                          ) 
				 	 when (n==5) (do
					  luvaEnemy1 <- findObject "luva1" "inimigos"
                                          moveLuva luvaEnemy1
				          luvaEnemy2 <- findObject "luva2" "inimigos" 
                                          moveLuva luvaEnemy2  
                                      	  luvaEnemy3 <- findObject "luva3" "inimigos" 
                                          moveLuva luvaEnemy3
                                          luvaEnemy4 <- findObject "luva4" "inimigos" 
                                          moveLuva luvaEnemy4
                                          luvaEnemy5 <- findObject "luva5" "inimigos" 
                                          moveLuva luvaEnemy5
                                          luvaEnemy6 <- findObject "luva6" "inimigos" 
                                          moveLuvaVertical luvaEnemy6
                                          luvaEnemy7 <- findObject "luva7" "inimigos" 
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
 					 if((getTilePictureIndex tile)==16) 
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


setNewMap :: Int -> JGMAction ()
setNewMap 2 = setCurrentMapIndex 1
setNewMap 3 = setCurrentMapIndex 2
setNewMap 4 = setCurrentMapIndex 3
setNewMap 5 = setCurrentMapIndex 4
setNewMap _ = return ()
 --tails <- getObjectsFromGroup "tail"
--colisao com espinho é morte
--colisão com objeto para , determinando chão
checkCollision :: JGMObject -> JGMAction ()
checkCollision bola = do
  bolaPos <- getObjectPosition bola
  tile <- getTileFromWindowPosition bolaPos
  inimigos <- getObjectsFromGroup "inimigos"
  col <- objectListObjectCollision inimigos bola
  
  when ((getTilePictureIndex tile)==20) (setObjectSpeed (speedMod-15,0) bola) -- outro elemento

  if ((getTileBlocked tile || col ))
          then (do setGameState GameOver
                   disableObjectsDrawing
                   disableObjectsMoving
                   setGameAttribute (GA defaultTimer objetivo (0,0) 0))
          else if (((getTilePictureIndex tile)==10) || ((getTilePictureIndex tile) == 11) || ((getTilePictureIndex tile) == 12) ||        ((getTilePictureIndex tile) == 13) || ((getTilePictureIndex tile) == 12))
          then stop
          else return()


l,f,s,x,y,w,z,gol :: JGMTile
f = (grass,   False, 0.0, NoTileAttribute)
s = (spike,True,0.0,NoTileAttribute)
x= (bl,False,0.0,NoTileAttribute)
y= (br,False,0.0,NoTileAttribute)
w= (bu,False,0.0,NoTileAttribute)
z= (bd,False,0.0,NoTileAttribute)
gol = (barra,False,0.0,NoTileAttribute)
l = (lama,False,0.0,NoTileAttribute)

map1 :: JGMMap
map1 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,f,f,f,f,y],
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
map2 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,gol,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]

map3 :: JGMMap
map3 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,s,s,s,f,gol,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]

map4 :: JGMMap
map4 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,f,f,f,f,s,s,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,gol,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]


map5 :: JGMMap	
map5 = [[s,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,s],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,s,s,s,s,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,s,s,f,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,s,s,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,s,f,gol,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,l,l,f,f,f,f,s,s,s,s,s,s],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,f,f,f,f,f,f,f,f,f,y],
        [x,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,s,s,s,f,f,y],
        [x,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,y],
        [x,l,l,l,l,l,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,l,l,l,s,s,y],
	[s,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,s]]	


