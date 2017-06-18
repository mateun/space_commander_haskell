{-# LANGUAGE OverloadedStrings #-}

module ShmupMain where

import SDL
import SDL.Raw.Basic as SRB
import SDL.Vect
import Foreign.C.Types
import Linear
import Control.Monad
import Textures
import Data.Text 
import qualified Data.Text.Lazy as L
import Foreign.C.String
import System.Directory
import Control.Concurrent
import System.Clock
import Formatting
import Formatting.Clock
import GHC.Int
import GHC.Word

-- When calling this function I get an error:
-- warning: failed to uninstall default console handler
-- and the console window hangs on end.
doLog :: String -> IO ()
doLog s = do
  msg <- newCString s
  SRB.log msg
  
data MoveCommand = MoveLeft | MoveRight | MoveUp | MoveDown | MoveNone

moveCmdForEvent :: EventPayload -> MoveCommand
moveCmdForEvent (KeyboardEvent keyboardEvent) = case keyboardEventKeyMotion keyboardEvent of
                                                   Pressed -> MoveLeft
moveCmdForEvent _ = MoveNone



-- We collect all movement commands of the current frame
-- into a list for further processing.
moveCmdForFrame :: IO [MoveCommand]
moveCmdForFrame = do
  ks <- getKeyboardState
  let cs = []
  let l = ks ScancodeA
  let cs' = case l of 
              True -> MoveLeft : cs;
              False -> MoveNone : cs
  let r = ks ScancodeD
  let cs'' = case r of 
               True -> (MoveRight : cs');
               False -> MoveNone : cs'
  let u = ks ScancodeW
  let cs''' = case u of { True -> MoveUp : cs''; False -> MoveNone : cs''}
  let d = ks ScancodeS
  let cs'''' = case d of 
                True -> MoveDown : cs''';
                False -> MoveNone : cs'''
  return cs''''
  
moveSpeed :: Float
moveSpeed = 4.0

-- Transforms a movement command to a V2.
transformCmdToVec :: MoveCommand -> V2 Float
transformCmdToVec MoveLeft = V2 (-moveSpeed) 0
transformCmdToVec MoveRight = V2 (moveSpeed) 0
transformCmdToVec MoveUp = V2 0 (-moveSpeed)
transformCmdToVec MoveDown = V2 0 moveSpeed
transformCmdToVec MoveNone = V2 0 0

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Shmup" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer { rendererType = AcceleratedVSyncRenderer }
  wd <- getCurrentDirectory
  playerTexture <- loadTexture renderer (wd ++ "/player_sprite_sheet.bmp")
  appLoop renderer playerTexture (V2 10 50) 1
  SDL.quit

appLoop :: Renderer -> Texture -> V2 Float -> CFloat -> IO()
appLoop renderer playerTex pos animFrame = do
  start <- getTime Monotonic
  events <- pollEvents
  let quit = elem SDL.QuitEvent $ Prelude.map eventPayload events
  let moveCmds = Prelude.map moveCmdForEvent $ Prelude.map eventPayload events
  let moveCmds' = [MoveRight, MoveUp]
  {-let eventIsQPress event = 
       case eventPayload event of 
          KeyboardEvent keyboardEvent ->
                 keyboardEventKeyMotion keyboardEvent == Pressed && 
                   keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
      -}

  moveCmds <- moveCmdForFrame
  let vecs = Prelude.map transformCmdToVec moveCmds
  let vecCondensed@(V2 xc yc) = Prelude.foldl (+) pos vecs
  let vecCondensedI = V2 (round xc) (round yc)
  let animFrame' = animFrame
  rendererDrawColor renderer $= V4 10 10 25 255
  clear renderer
  renderTexture renderer playerTex (Just (Rectangle (P (V2 0 5)) (V2 64 64)))
                                 (Just (Rectangle (P (vecCondensedI)) (V2 64 64)))
  present renderer
  end <- getTime Monotonic
  -- let diff = (((nsec end) - (nsec start)) `div` 1000 `div` 1000)
  -- not necessary due to VSYNC
  --SDL.delay (getWaitPeriod (fromIntegral diff))
  unless quit (appLoop renderer playerTex vecCondensed animFrame')
  
  
  
  
  
  
  