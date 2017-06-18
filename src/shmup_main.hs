{-# LANGUAGE OverloadedStrings #-}

import SDL
import SDL.Raw.Basic as SRB
import SDL.Vect
import Foreign.C.Types
import Linear
import Control.Monad
import Textures
import Data.Text 
import Foreign.C.String
import System.Directory

-- When calling this function I get an error:
-- warning: failed to uninstall default console handler
-- and the console window hangs on end.
doLog :: String -> IO ()
doLog s = do
  msg <- newCString s
  SRB.log msg

main :: IO ()
main = do
  --doLog "starting up!"
  initializeAll
  window <- createWindow "Shmup" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  wd <- getCurrentDirectory
  --doLog ( wd)
  playerTexture <- loadTexture renderer (wd ++ "/player.bmp")
  --simpleLoop renderer
  appLoop renderer playerTexture
  SDL.quit


simpleLoop :: Renderer -> IO()
simpleLoop r = do
  events <- pollEvents
  let quit = elem SDL.QuitEvent $ Prelude.map eventPayload events
  rendererDrawColor r $= V4 10 10 25 255
  clear r
  present r
  unless quit (simpleLoop r)
  
  

appLoop :: Renderer -> Texture -> IO()
appLoop renderer playerTex = do
  events <- pollEvents
  let quit = elem SDL.QuitEvent $ Prelude.map eventPayload events
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