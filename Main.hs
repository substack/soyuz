{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.UI.GLUT hiding (Matrix(..),newMatrix,rotate,translate)
import qualified Graphics.UI.GLUT as GL
import qualified Graphics.GD as GD

import Numeric.LinearAlgebra.Transform
import Numeric.LinearAlgebra hiding (scale,reshape)

import Control.Concurrent (newMVar,takeMVar,putMVar,threadDelay)
import qualified Data.Set as Set

import Control.Monad (liftM2,forM_,mapM_)
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&),first,second)
import Data.List.Split (splitOn)
import Data.List
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

type GroundSmoke = [(GLfloat,GLfloat)]

data State = State {
    keySet :: Set.Set Key,
    mousePos :: (Int,Int),
    mousePrevPos :: (Int,Int),
    cameraMatrix :: Matrix Double,
    paused :: Bool,
    following :: Bool,
    wireframe :: Bool,
    soyuzTex :: Tex,
    soyuzSolid :: SolidData,
    soyuzJet :: Jet,
    soyuzHeight :: GLfloat,
    groundSmoke :: GroundSmoke
} deriving Show

toGLmat :: (Real e, Num (Matrix e), Linear Matrix e)
    => Matrix e -> IO (GLmatrix GLdouble)
toGLmat = GL.newMatrix RowMajor
    . map (fromRational . toRational)
    . concat . toLists

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

imageLoader :: FilePath -> IO GD.Image
imageLoader file | ".png" `isSuffixOf` file = GD.loadPngFile file
imageLoader file | ".jpg" `isSuffixOf` file = GD.loadJpegFile file
imageLoader file = fail $ "Unknown image extension in file: " ++ file

readTex :: FilePath -> IO Tex
readTex file = do
    im <- imageLoader file
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
    initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer ]
    initialWindowSize $= Size 500 500
    initialWindowPosition $= Position 0 0
    createWindow "soyuz-u"
    
    depthMask $= Enabled
    depthFunc $= Just Less
    blend $= Enabled
    blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
    
    shadeModel $= Smooth
    lighting $= Disabled
    
    solid <- readObj "soyuz-u.obj"
    putStrLn "Loading soyuz texture"
    sTex <- readTex "soyuz-u-texture.png"
    putStrLn "Loading terrain texture"
    
    putStrLn "Initializing"
    
    stateVar <- newMVar $ State {
        keySet = Set.empty,
        mousePos = (0,0),
        mousePrevPos = (0,0),
        cameraMatrix = translate (3 |> [0,-40,-20])
            $ rotation (AxisX (pi / 4)),
        soyuzTex = sTex,
        soyuzSolid = solid,
        soyuzJet = [],
        soyuzHeight = 0,
        paused = False,
        following = False,
        wireframe = False,
        groundSmoke = []
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
        perspective 60 as 1 10000
    
    mainLoop

onKeyUp :: State -> Key -> IO State
onKeyUp state (Char ' ') = return
    $ state { soyuzJet = [], soyuzHeight = 0, groundSmoke = [] }
onKeyUp state (Char 'p') = return $ state { paused = not (paused state) }
onKeyUp state (Char 'f') = return $ state { following = not (following state) }
onKeyUp state (Char 'o') = return $ state { wireframe = not (wireframe state) }
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
        
        mouseButton b = Set.member (MouseButton b) (keySet state)
        
        matF mat
            -- left button pans left and right, up and down
            | mouseButton LeftButton =
                rotation (AxisX dy) <> rotation (AxisY dx) <> mat
            -- right button skewers
            | mouseButton RightButton = rotation (AxisZ (dx + dy)) <> mat
            | otherwise = mat
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
    startT <- get elapsedTime
    
    clearColor $= Color4 0.75 0.8 0.85 1
    clear [ ColorBuffer, DepthBuffer ]
    
    matrixMode $= Modelview 0
    loadIdentity
    
    multMatrix =<< (toGLmat $ cameraMatrix state)
    
    renderRocket state
    
    flush
    swapBuffers
    postRedisplay Nothing
    
    dt <- (subtract startT) <$> get elapsedTime
    threadDelay $ max 0 (75 - dt)
    
    (navigate <$>)
        $ if paused state
            then return state
            else stepHeight <$> (stepSmoke =<< stepJet state)

renderRocket :: State -> IO ()
renderRocket state = do
    let
        ((xmin,xmax),(ymin,ymax),(zmin,zmax)) = solidBounds $ soyuzSolid state
        
        triM, quadM :: MFace -> IO ()
        triM (TriF a b c) = mapM_ ptM [a,b,c]
        triM _ = return ()

        quadM (QuadF a b c d) = mapM_ ptM [a,b,c,d]
        quadM _ = return ()

        ptM :: VN -> IO ()
        ptM ((vx,vy,vz),(nx,ny,nz)) = do
            color $ Color4 1 1 1 (1 :: GLfloat)
            normal $ Normal3 nx ny nz
            let tx = (vx - xmin) / (xmax - xmin)
                ty = (vy - ymin) / (ymax - ymin)
                tz = (vz - zmin) / (zmax - zmin)
                xz = (pi + atan2 tx ty) / (2 * pi)
            texCoord $ TexCoord2 xz ty
            vertex $ Vertex3 vx vy vz
     
    preservingMatrix $ do
        -- first render the stuff on the ground
        renderTerrain state
        
        ($ state)
            $ if wireframe state
                then renderWireSmoke
                else renderSolidSmoke
        
        -- then render the stuff in the sky
        GL.translate $ Vector3 0 (-ymin + soyuzHeight state) 0
         
        withTexture2D (soyuzTex state) $ do
            renderPrimitive Triangles
                $ mapM_ triM (solidFaces $ soyuzSolid state)
            renderPrimitive Quads
                $ mapM_ quadM (solidFaces $ soyuzSolid state)
        
        GL.translate $ Vector3 0 ymin 0
        
        ($ soyuzJet state)
            $ if wireframe state
                then renderWireJet
                else renderSolidJet

-- render the terrain as a series of wedges with an image on top
renderTerrain :: State -> IO ()
renderTerrain state = renderPrimitive Triangles $ do
    let dt = 2 * pi / 24
    forM_ [ 0, dt .. 2 * pi ] $ \theta -> do
        color $ (Color4 0.6 0.55 0.4 1 :: Color4 GLfloat)
        normal (Normal3 0 0 1 :: Normal3 GLfloat)
        let theta' = theta + dt
            r = 100
            y = 0
            
            x0 = r * cos theta
            z0 = r * sin theta
            tx0 = 100 * x0 / r
            tz0 = 100 * z0 / r
            
            x1 = r * cos theta'
            z1 = r * sin theta'
            tx1 = 100 * x1 / r
            tz1 = 100 * z1 / r
        
        texCoord $ TexCoord2 tx0 tz0
        vertex (Vertex3 x0 y z0 :: Vertex3 GLfloat)
        
        texCoord $ TexCoord2 tx1 tz1
        vertex (Vertex3 x1 y z1 :: Vertex3 GLfloat)
        
        texCoord (TexCoord2 0.5 0.5 :: TexCoord2 GLfloat)
        vertex (Vertex3 0 y 0 :: Vertex3 GLfloat)

stepHeight :: State -> State
stepHeight state = if following state then state'' else state'
    where
        state' = state { soyuzHeight = (soyuzHeight state) + 0.5 }
        state'' = state' { cameraMatrix = tr (cameraMatrix state) }
        tr = translate (3 |> [0,-0.5,0])

stepSmoke :: State -> IO State
stepSmoke state = do
    gDzs <- newStdGen
    let
        dzs = map ftr (randomRs (-0.2,0.2) gDzs :: [Float])
        ftr = fromRational . toRational
        
        smokes = (0.01, -0.5) :
            zipWith f (groundSmoke state) dzs
        f (r,z) dz = (r + 0.4, min (-0.01) (z - dz))
        
        h = soyuzHeight state
        takeN = floor $ 100 - h
    return $ state { groundSmoke = take takeN smokes }

stepJet :: State -> IO State
stepJet state = do
    [r,a,o,z] <- mapM ((fromRational . toRational <$>) . randomRIO)
        ([(1.5,1.7),(0,2*pi),(0.0,0.3),(0.1,0.3)] :: [(Float,Float)])
    let seg = Segment {
            segmentRadius = r,
            segmentAngle = a,
            segmentOffset = o,
            segmentZ = z
        }
        
        stepZ :: GLfloat -> Jet -> Jet
        stepZ dz = map (\s -> s { segmentZ = (segmentZ s) + dz })
        
        taper :: Jet -> Jet
        taper = map (\s -> s { segmentRadius = (segmentRadius s) * dr })
            where dr = 0.97
        
        jet' = take 40 $ (seg :) $ taper $ stepZ 1.2 $ soyuzJet state
        
    return $ state { soyuzJet = jet' }

renderJet :: PrimitiveMode -> Jet -> IO ()
renderJet mode jet = renderPrimitive mode
    $ mapM_ f $ zip3 [0..] jet (tail jet)
    where
        f :: (GLfloat,Segment,Segment) -> IO ()
        f (i,s1,s2) = forM_ (lathe 12 s1 s2) $ \(QuadF pn1 pn2 pn3 pn4) ->
            forM_ [pn1,pn2,pn3,pn4] $ \((vx,vy,vz),(nx,ny,nz)) -> do
                let alpha = 1 - i / 20
                color $ Color4 1 1 1 alpha
                normal $ Normal3 nx ny nz
                vertex $ Vertex3 vx (-vz) vy
renderSolidJet jet = do
    -- render some triangles on the bottom of the thrusters
    renderPrimitive Triangles $ do
        let dt = 2 * pi / 8
        forM_ [ 0, dt .. 2 * pi ] $ \theta -> do
            color (Color4 1 1 1 1 :: Color4 GLfloat)
            let theta' = theta + dt
                (x,z) = (r * cos theta, r * sin theta)
                (x',z') = (r * cos theta', r * sin theta')
                r = 1.5
                y = -0.1
            vertex (Vertex3 x y z :: Vertex3 GLfloat)
            vertex (Vertex3 x' y z' :: Vertex3 GLfloat)
            vertex (Vertex3 0 y 0 :: Vertex3 GLfloat)
    renderJet Quads jet
renderWireJet = renderJet Lines

renderSmoke :: PrimitiveMode -> State -> IO ()
renderSmoke mode state = renderPrimitive mode $ do
    let
        smoke = groundSmoke state
        dt = 2 * pi / 18
        coords = liftM2 (,) (take 18 $ iterate (+dt) 0) smoke
        coordPairs = zip coords (tail coords ++ [head coords])
        h = soyuzHeight state
    -- connect each radial fan to its neighbor
    forM_ coordPairs $ \((t,(r,z)),(_,(r',z'))) -> do
        let
            t' = t + dt
            (x0,y0) = (r * cos t, r * sin t)
            (x1,y1) = (r' * cos t, r' * sin t)
            (x2,y2) = (r' * cos t', r' * sin t')
            (x3,y3) = (r * cos t', r * sin t')
            alpha = (1 - r / 40) - (sqrt $ h / 60)
        
        color $ Color4 1 1 1 alpha
        vertex $ Vertex3 x0 (-z) y0
        vertex $ Vertex3 x1 (-z') y1
        vertex $ Vertex3 x2 (-z') y2
        vertex $ Vertex3 x3 (-z) y3
renderSolidSmoke = renderSmoke Quads
renderWireSmoke = renderSmoke Lines

lathe :: Int -> Segment -> Segment -> [MFace]
lathe n seg1 seg2 =
    [
        let [pn1,pn2,pn3,pn4] = map (flip (,) $ norm) $ pts a
            norm = surfaceNorm (pts a)
        in QuadF pn1 pn2 pn3 pn4
    | a <- [ 0, da .. 2 * pi ] ]
    where
        pts a = [
                (ox1 + r1 * cos a, oy1 + r1 * sin a, oz1),
                (ox1 + r1 * cos (a + da), oy1 + r1 * sin (a + da), oz1),
                (ox2 + r2 * cos (a + da), oy2 + r2 * sin (a + da), oz2),
                (ox2 + r2 * cos a, oy2 + r2 * sin a, oz2)
            ] 
        (ox1,oy1,oz1) = offset seg1
        (ox2,oy2,oz2) = offset seg2
        da = 2 * pi / (fromIntegral n)
        r1 = segmentRadius seg1
        r2 = segmentRadius seg2
 
surfaceNorm :: [V] -> V
surfaceNorm vs = (0,1,0) -- fixed normal for now

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
