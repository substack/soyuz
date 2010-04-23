{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.UI.GLUT hiding (Matrix(..),newMatrix,rotate,translate)
import qualified Graphics.UI.GLUT as GL
import qualified Graphics.GD as GD

import Numeric.LinearAlgebra.Transform
import Numeric.LinearAlgebra hiding (scale,reshape)

import Control.Concurrent (newMVar,takeMVar,putMVar)
import qualified Data.Set as Set

import Control.Monad (liftM2,forM_,mapM_)
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import System.Random (randomRIO,randomRs,newStdGen)

import Foreign (newArray)

type V = (GLfloat,GLfloat,GLfloat)
type VN = (V,V)

data MFace = TriF VN VN VN | QuadF VN VN VN VN
    deriving Show

type Bound = (GLfloat,GLfloat)

data SolidData = SolidData {
    solidFaces :: [MFace],
    solidBounds :: (Bound,Bound,Bound)
} deriving Show

type TexData = PixelData (Color4 GLfloat)
data Tex = Tex { texData :: TexData, texSize :: GD.Size }
    deriving Show

type Jet = [Segment]
data Segment = Segment {
    segmentRadius :: GLfloat,
    segmentAngle :: GLfloat,
    segmentOffset :: GLfloat,
    segmentZ :: GLfloat
} deriving Show

data State = State {
    keySet :: Set.Set Key,
    mousePos :: (Int,Int),
    mousePrevPos :: (Int,Int),
    cameraMatrix :: Matrix Double,
    soyuzTex :: Tex,
    soyuzSolid :: SolidData,
    soyuzJet :: Jet
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

readObj :: FilePath -> IO SolidData
readObj file = do
    rows <- map words . lines <$> readFile file
    let
        tup2 [x,y] = (x,y)
        tup3 [x,y,z] = (x,y,z)
        
        verts, norms :: [V]
        verts = map (tup3 . map read . tail) $ filter ((== "v") . head) rows
        norms = map (tup3 . map read . tail) $ filter ((== "vn") . head) rows
        
        faceIx :: [[(Int,Int)]]
        faceIx = map (map (tup2 . map (pred . read) . splitOn "//") . tail)
            $ filter ((== "f") . head) rows
        faces :: [MFace]
        faces = map f faceIx where
            f [x,y,z] = TriF (g x) (g y) (g z)
            f [x,y,z,w] = QuadF (g x) (g y) (g z) (g w)
            g :: (Int,Int) -> (V,V)
            g (i,j) = (verts !! i, norms !! j)
        
        bounds = (
                minimum &&& maximum $ map (\(x,_,_) -> x) verts,
                minimum &&& maximum $ map (\(_,y,_) -> y) verts,
                minimum &&& maximum $ map (\(_,_,z) -> z) verts
            )
    return $ SolidData { solidFaces = faces, solidBounds = bounds }

readTex :: FilePath -> IO Tex
readTex file = do
    im <- GD.loadPngFile file
    (width,height) <- GD.imageSize im
    let
        colorize :: GD.Color -> Color4 GLfloat
        colorize i = Color4 r g b 1.0 where
            r = (fromIntegral $ i `div` (256 * 256)) / 256
            g = (fromIntegral $ (i `div` 256) `mod` 256) / 256
            b = (fromIntegral $ i `mod` 256) / 256
        getPix :: GD.Point -> IO (Color4 GLfloat)
        getPix (x,y) = colorize <$> GD.getPixel (x,y) im
         
        xy = liftM2 (,) [0..width-1] [0..height-1]
    tData <- (PixelData RGBA Float <$>) . newArray =<< mapM getPix xy
    return $ Tex { texData = tData, texSize = (width,height) }

main :: IO ()
main = do
    (_, argv) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 400 300
    initialWindowPosition $= Position 0 0
    createWindow "soyuz-u"
    depthMask $= Enabled
    depthFunc $= Nothing
    
    shadeModel $= Smooth
    lighting $= Disabled
    
    solid <- readObj "soyuz-u.obj"
    tex <- readTex "soyuz-u-texture.png"
    
    stateVar <- newMVar $ State {
        keySet = Set.empty,
        mousePos = (0,0),
        mousePrevPos = (0,0),
        cameraMatrix = translation $ 3 |> [0,0,-20],
        soyuzTex = tex,
        soyuzSolid = solid,
        soyuzJet = []
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
        putMVar stateVar =<< (f <$>) . onMouseMove True =<< takeMVar stateVar
    
    (passiveMotionCallback $=) . Just $ \pos -> do
        let Position posX posY = pos
            f state = state {
                mousePos = (fromIntegral posX, fromIntegral posY),
                mousePrevPos = mousePos state
            }
        putMVar stateVar =<< (f <$>) . onMouseMove False =<< takeMVar stateVar
    
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
        perspective 60 as 10000 0
    
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

onMouseMove :: Bool -> State -> IO State
onMouseMove True state = return
    $ state { cameraMatrix = matF (cameraMatrix state) }
    where
        mpos = mousePos state
        ppos = mousePrevPos state
        dt = 0.002
        dx = dt * (fromIntegral $ fst ppos - fst mpos)
        dy = dt * (fromIntegral $ snd ppos - snd mpos)
        matF mat = rotation (AxisX dy) <> rotation (AxisY dx) <> mat
onMouseMove False state = return state

navigate :: State -> State
navigate state = state { cameraMatrix = matF (cameraMatrix state) }
    where
        keys = Set.elems $ keySet state
        matF mat = case keys of
            [] -> mat
            _ -> translation tsum <> mat
                where tsum = sum $ map ((3 |>) . tKey) keys
        dt = 0.5
        
        tKey :: Key -> [Double]
        tKey (Char 'w') = [0,0,dt] -- forward
        tKey (Char 's') = [0,0,-dt] -- back
        tKey (Char 'a') = [dt,0,0] -- strafe left
        tKey (Char 'd') = [-dt,0,0] -- strafe right
        tKey (Char 'q') = [0,-dt,0] -- up
        tKey (Char 'z') = [0,dt,0] -- down
        tKey _ = [0,0,0]

display :: State -> IO State
display state = do
    clearColor $= Color4 0 0 0 1
    clear [ ColorBuffer ]
    
    matrixMode $= Modelview 0
    loadIdentity
    
    multMatrix =<< (toGLmat $ cameraMatrix state)
    
    let triM, quadM :: MFace -> IO ()
        triM (TriF a b c) = mapM_ ptM [a,b,c]
        triM _ = return ()
        
        quadM (QuadF a b c d) = mapM_ ptM [a,b,c,d]
        quadM _ = return ()
        
        coords = [
                TexCoord3 0 0 0,
                TexCoord3 0 1 0,
                TexCoord3 1 1 1,
                TexCoord3 1 0 1
            ] :: [TexCoord3 GLfloat]
        
        (texW,texH) = (fromIntegral $ fst size, fromIntegral $ snd size)
        size = texSize (soyuzTex state)
        
        ((xmin,xmax),(ymin,ymax),(zmin,zmax)) = solidBounds $ soyuzSolid state
        
        ptM :: VN -> IO ()
        ptM ((vx,vy,vz),(nx,ny,nz)) = do
            color $ Color4 1 1 1 (1 :: GLfloat)
            normal $ Normal3 nx ny nz
            let tx = (vx - xmin) / (xmax - xmin)
                ty = (vy - ymin) / (ymax - ymin)
            texCoord $ TexCoord2 tx ty
            vertex $ Vertex3 vx vy vz
     
    withTexture2D (soyuzTex state) $ do
        renderPrimitive Triangles $ mapM_ triM (solidFaces $ soyuzSolid state)
        renderPrimitive Quads $ mapM_ quadM (solidFaces $ soyuzSolid state)
    
    renderJet $ soyuzJet state
    
    flush
    swapBuffers
    postRedisplay Nothing
    
    stepJet $ navigate state

stepJet :: State -> IO State
stepJet state = do
    [r,a,o,z] <- mapM ((fromRational . toRational <$>) . randomRIO)
        ([(1.0,1.2),(0,2*pi),(0,1),(0,0.5)] :: [(Float,Float)])
    let seg = Segment {
            segmentRadius = r,
            segmentAngle = a,
            segmentOffset = o,
            segmentZ = z
        }
    segs <- zipWith
        (\s dz -> s { segmentZ = (segmentZ s) + dz })
        (take 49 $ soyuzJet state)
        . map (fromRational . toRational)
        . randomRs (0.08,0.12 :: Float) <$> newStdGen
    return $ state { soyuzJet = seg : segs }

renderJet :: Jet -> IO ()
renderJet jet = renderPrimitive Quads $ mapM_ f $ zip jet (tail jet) where
    f :: (Segment,Segment) -> IO ()
    f (s1,s2) = undefined

lathe :: Int -> Segment -> Segment -> [MFace]
lathe n seg1 seg2 =
    [
        let [pn1,pn2,pn3,pn4] = map (flip (,) $ norm) $ pts a
            norm = surfaceNorm (pts a)
        in QuadF pn1 pn2 pn3 pn4
    | a <- [ 0, da .. 2 * pi ] ]
    where
        pts a = [
                (r1 * cos a, r1 * sin a, z1),
                (r1 * cos (a + da), r1 * sin (a + da), z1),
                (r2 * cos (a + da), r2 * sin (a + da), z2),
                (r2 * cos a, r2 * sin a, z2)
            ] 
        da = (fromIntegral n) / (2 * pi)
        r1 = segmentRadius seg1
        r2 = segmentRadius seg2
        z1 = segmentZ seg1
        z2 = segmentZ seg2
 
surfaceNorm :: [V] -> V
surfaceNorm vs = undefined

offset :: Segment -> V
offset seg = (x,y,segmentZ seg) where
    x = r * cos a
    y = r * sin a
    a = segmentAngle seg
    r = segmentOffset seg

withTexture2D :: Tex -> IO () -> IO ()
withTexture2D Tex{ texData = tData, texSize = (w',h') } f = do
    let size = TextureSize2D (fromIntegral w') (fromIntegral h')
    
    -- save texture capability
    prevCap <- get $ texture Texture2D
    
    rowAlignment Unpack $= 1
    texture Texture2D $= Enabled
    
    [tex] <- genObjectNames 1
    textureBinding Texture2D $= Just tex
    textureFunction $= Decal
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texImage2D Nothing NoProxy 0  RGBA' size 0 tData
    
    f -- user geometry
    
    -- set texture capability back to whatever it was before
    texture Texture2D $= prevCap
