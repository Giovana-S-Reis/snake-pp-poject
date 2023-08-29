module Main where

import Snake
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

window :: Display
window = InWindow "Cobraskell" (940, 740) (100, 100)

background :: Color
background = white

seta :: Picture
seta = pictures $ [color black $ line [(0,-10), (0,10)]]
               ++ [color black $ polygon [(5,0), (0, 10), (-5, 0)]]

-- | Desenha as instruções do jogo na tela.
instrucoes :: Picture
instrucoes = pictures $ [translate (-480) (-25) seta]
                     ++ [translate (-525) (-40) $ rotate (-90) seta]
                     ++ [translate (-480) (-60) $ rotate 180 seta]
                     ++ [translate (-445) (-40) $ rotate 90 seta]
                     ++ [translate (-605) (-35)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para")]
                     ++ [translate (-600) (-50)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "esquerda")]
                     ++ [translate (-422) (-35)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para")]
                     ++ [translate (-410) (-50)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "direita")]
                     ++ [translate (-510) (10)   (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para")]
                     ++ [translate (-495) (-5)   (scale 0.1 0.1 $ color (greyN 0.5) $ Text "cima")]
                     ++ [translate (-515) (-90)  (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Move para")]
                     ++ [translate (-495) (-105) (scale 0.1 0.1 $ color (greyN 0.5) $ Text "baixo")]
                     ++ [translate (-195) (300) (scale 0.1 0.1 $ color (greyN 0.5) $ Text "Mova a Cobraskell para que ela encontre a comida e se alimente")]



render :: GameState -> Picture
render gameState =
    if isInitialScreen gameState
        then pictures $ [translate (-200) 45 $ scale 0.5 0.5 $ text "Cobraskell!!"]
		             ++ [translate (-500) (-10) $ scale 0.5 0.5 $ text "Pressione a barra de espaco"]
					 ++ [translate (-200) (-75) $ scale 0.5 0.5 $ text "para iniciar"]
        else pictures $   [ fillRectangle black (16, 0) (640, 20)
                          , fillRectangle black (16, 24) (640, 20)
                          , fillRectangle black (0, 12) (20, 480)
                          , fillRectangle black (32, 12) (20, 480) ]
                            ++ fmap (convertToPicture black) snake 
                            ++ fmap (convertToPicture blue) [food]
                            ++ gameOverPicture
                            ++ [instrucoes]
  where
    snake = getSnake gameState 
    food = getFood gameState
    convertToPicture :: Color -> (Int, Int) -> Picture
    convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
    fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                            scale 1 (-1) $ 
                                            translate (tx * 20 - 320) (ty * 20 - 240) $ 
                                            rectangleSolid w h
    toFloat (x, y) = (fromIntegral x, fromIntegral y)
    gameOverPicture = if isGameOver gameState
          then [ color blue $
                 translate (-200) (0) $
                 scale 0.5 0.5 $
                 text "GAME OVER"
               , color blue $
                 translate (-175) (-50) $
                 scale 0.2 0.2 $
                 text ("Pontuação: " ++ show (getScore gameState))
               , color blue $
                 translate (-175) (-100) $
                 scale 0.2 0.2 $
                 text "Press SPACE to try again."
               ]
          else []




                                                        
update :: Float -> GameState -> GameState
update seconds gameState
    | isInitialScreen gameState = gameState
    | isGameOver gameState = gameState
    | isSpacePressed gameState = initialGameState False
    | otherwise = gameState'
  where
    snake = getSnake gameState
    food = getFood gameState
    direction = getDirection gameState
    gameOver = isGameOver gameState
    stdGen = getRandomStdGen gameState
    (wasFoodEaten, newSnake) = move food direction snake
    (newFood, newStdGen) = generateNewFood newSnake stdGen
    newFood' = if wasFoodEaten
        then newFood
        else food
    newGameOver = checkGameOver newSnake
    newScore = if wasFoodEaten
        then getScore gameState + 1
        else getScore gameState
    gameState' = GameState newSnake newFood' direction newGameOver newStdGen newScore False (isSpacePressed gameState)



handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState = changeDirection gameState LEFT
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = changeDirection gameState RIGHT
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = changeDirection gameState UP
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState = changeDirection gameState DOWN
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState
    | isInitialScreen gameState = initialGameState False
    | otherwise = gameState { isSpacePressed = True }
handleKeys _ gameState = gameState { isSpacePressed = False }



main :: IO ()
main = play window background 10 (initialGameState False) render handleKeys update


