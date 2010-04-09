{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.UI.GLUT hiding (Matrix(..),newMatrix,rotate,translate)
import qualified Graphics.UI.GLUT as GL
import Numeric.LinearAlgebra.Transform
import Numeric.LinearAlgebra hiding (scale,reshape)

import Control.Concurrent (newMVar,takeMVar,putMVar)
import qualified Data.Set as Set

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first,second,(&&&),(***))
import Control.Monad (when,forM_,join)
import Control.Monad.Error (runErrorT)
import Data.Maybe (fromJust,isJust,isNothing)

import System.Process (system)
import System.Exit (ExitCode(..))

data State = State {
    keySet :: Set.Set Key,
    mousePos :: (Int,Int),
    mousePrevPos :: (Int,Int),
    cameraMatrix :: Matrix Double
} deriving Show

toGLmat :: (Real e, Num (Matrix e), Linear Matrix e)
    => Matrix e -> IO (GLmatrix GLdouble)
toGLmat = GL.newMatrix RowMajor
    . map (fromRational . toRational)
    . concat . toLists

translateM :: MatrixComponent c => Vector3 c -> IO ()
translateM = GL.translate

rotateM :: MatrixComponent c => c -> Vector3 c -> IO ()
rotateM = GL.rotate

main :: IO ()
main = do
    (_, argv) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 400 300
    initialWindowPosition $= Position 0 0
    createWindow "soyuz-u"
    depthMask $= Enabled
    lighting $= Disabled
    
    stateVar <- newMVar $ State {
        keySet = Set.empty,
        mousePos = (0,0),
        mousePrevPos = (0,0),
        cameraMatrix = ident 4
    }
    
    actionOnWindowClose $= MainLoopReturns
    
    (keyboardMouseCallback $=) . Just $ \key keyState modifiers pos -> do
        state <- takeMVar stateVar
        state' <- ($ key) 
            $ ($ keyboard state key keyState modifiers pos)
            $ case keyState of
                Up -> onKeyUp
                Down -> onKeyDown
        putMVar stateVar state'
    
    (motionCallback $=) . Just $ \pos -> do
        let Position posX posY = pos
            f state = state {
                mousePos = (fromIntegral posX, fromIntegral posY),
                mousePrevPos = mousePos state
            }
        putMVar stateVar =<< (f <$> takeMVar stateVar)
    
    (passiveMotionCallback $=) . Just $ \pos -> do
        let Position posX posY = pos
            f state = state {
                mousePos = (fromIntegral posX, fromIntegral posY),
                mousePrevPos = mousePos state
            }
        putMVar stateVar =<< (f <$> takeMVar stateVar)
    
    (displayCallback $=) $ do
        state <- display =<< takeMVar stateVar
        putMVar stateVar state
        return ()
    
    (reshapeCallback $=) . Just $ \size@(Size w h) -> do
        viewport $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        let as = fromIntegral w / fromIntegral h
            sa = recip as
        perspective 60 as 0 10000
    
    mainLoop

onKeyUp :: State -> Key -> IO State
-- print the state
onKeyUp state (Char ' ') = print state >> return state
onKeyUp state key = return state

onKeyDown :: State -> Key -> IO State
-- escape key exits application
onKeyDown state (Char '\27') = leaveMainLoop >> return state
onKeyDown state key = return state

keyboard :: State -> Key -> KeyState -> Modifiers -> Position -> State
keyboard state key keyState modifiers pos = state'
    where
        state' = state { keySet = f key (keySet state) }
        f = case keyState of
            Up -> Set.delete
            Down -> Set.insert

navigate :: State -> State
navigate state = state { cameraMatrix = matF (cameraMatrix state) }
    where
        mpos = mousePos state
        ppos = mousePrevPos state
        keys = Set.elems $ keySet state
        
        matF = \mat -> ($ mat) $ case keys of
            [] -> id
            _ -> ((foldl1 (<>) $ map rKey keys) <> tMat <>)
        tMat = translation (sum $ map tKey keys)
        
        dt = 0.1
        drx = -0.1 * (fromIntegral $ fst mpos - fst ppos)
        dry = -0.1 * (fromIntegral $ snd mpos - snd ppos)
        
        tKey :: Key -> Vector Double
        tKey (Char 'w') = 3 |> [0,0,dt] -- forward
        tKey (Char 's') = 3 |> [0,0,-dt] -- back
        tKey (Char 'a') = 3 |> [dt,0,0] -- strafe left
        tKey (Char 'd') = 3 |> [-dt,0,0] -- strafe right
        tKey (Char 'q') = 3 |> [0,-dt,0] -- up
        tKey (Char 'z') = 3 |> [0,dt,0] -- down
        tKey _ = 3 |> [0,0,0]
        
        rKey :: Key -> Matrix Double
        rKey (MouseButton LeftButton) = r1 <> r2
            where
                r1 = rotation (AxisX dry)
                r2 = rotation (AxisY drx)
        rKey (MouseButton RightButton) = rotation (AxisZ drx)
        rKey _ = ident 4

display :: State -> IO State
display state = do
    clearColor $= Color4 0 0 0 1
    clear [ ColorBuffer ]
    
    matrixMode $= Modelview 0
    loadIdentity
    
    flush
    swapBuffers
    postRedisplay Nothing
    
    return $ navigate state
