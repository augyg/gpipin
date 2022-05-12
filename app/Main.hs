{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}   

module Main where

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Exception

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Hello Gays!")

    let triangleArray = [ (V4 (-1) (-1) 0 1, V3 0 0 1)
                        , (V4 0 1 0 1, V3 0 1 0)
                        , (V4 1 (-1) 0 1,  V3 1 0 0)
                        ]

    -- let squareArray = [ (V4 (-1) (-1) 0 1, V3 0 0 1)
    --                   , (V4 1 1 0 1, V3 0 1 0)
    --                   , (V4 1 (-1) 0 1,  V3 1 0 0)
    --                   , (V4 (-1) 1 0 1, V3 0 1 0)
    --                   ]

    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 3
    writeBuffer vertexBuffer 0 triangleArray

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer shader win

loop :: (Control.Monad.IO.Class.MonadIO m,
         Control.Monad.Exception.MonadException m,
         ContextColorFormat c, Num a1, Color c Float ~ V3 a1) =>
         Buffer os a2
         -> (PrimitiveArray Triangles a2 -> Render os ())
         -> Window os c ds
         -> ContextT Handle os m ()

loop vertexBuffer shader win = do
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer shader win   
