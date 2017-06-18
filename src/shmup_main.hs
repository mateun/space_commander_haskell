{-# LANGUAGE OverloadedStrings #-}

import SDL
import SDL.Vect
import Foreign.C.Types
import Linear
import Control.Monad
import Textures

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Shmup" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  playerTexture <- loadTexture renderer "C:\\Users\\martin\\Documents\\Projects\\Haskell\\games\\shmup\\dist\\player.bmp"
  appLoop renderer playerTexture

appLoop :: Renderer -> Texture -> IO()
appLoop renderer playerTex = do
  events <- pollEvents
  let quit = elem SDL.QuitEvent $ map eventPayload events
  {-let eventIsQPress event = 
       case eventPayload event of 
          KeyboardEvent keyboardEvent ->
                 keyboardEventKeyMotion keyboardEvent == Pressed && 
                   keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
      -}

  rendererDrawColor renderer $= V4 10 10 25 255
  clear renderer
  renderTexture renderer playerTex Nothing
                                   (Just (Rectangle (P (V2 20 20)) (V2 64 64)))
  present renderer
  unless quit (appLoop renderer playerTex)