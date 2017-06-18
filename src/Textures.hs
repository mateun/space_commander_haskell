module Textures where

import SDL
import Linear
import Foreign.C.Types

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  -- set white as our default transparent key
  let key = V4 255 255 255 255
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  freeSurface surface
  return t


renderTexture :: SDL.Renderer -> Texture -> Maybe (Rectangle CInt) -> Maybe (Rectangle CInt) -> IO ()
renderTexture r t sr dr = do
    --let dstRect = Rectangle (P (V2 10 10)) (V2 64 64)
    SDL.copy r t sr dr