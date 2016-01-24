{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Halive.Utils
import Control.Monad.Reader
import Data.Time

data Uniforms = Uniforms 
    { uMVP :: UniformLocation (M44 GLfloat) 
    } deriving Data

main :: IO ()
main = do
    (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768
  
    shader        <- createShaderProgram "app/geo.vert" "app/geo.frag"
    Uniforms{..}  <- acquireUniforms shader
  
    icoGeo     <- icosahedronGeometry 0.5 5
    icoShape   <- makeShape icoGeo shader
  
    cubeGeo    <- cubeGeometry 1 5
    cubeShape  <- (makeShape cubeGeo shader :: IO (Shape Uniforms))
    
    planeGeo   <- planeGeometry 1 (V3 0 0 1) (V3 0 1 0) 5
    planeShape <- makeShape planeGeo shader
  
    let shapes = [ (cubeShape, V3 (-1) 0 0)
                 , (icoShape , V3 1 0 0)
                 , (planeShape, V3 0 (-1) 0)
                 ]
  
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
  
    whileWindow win $ do
        projection <- getWindowProjection win 45 0.1 1000
        let view = viewMatrix (V3 0 0 5) (axisAngle (V3 0 1 0) 0)
        (x,y,w,h) <- getWindowViewport win
        glViewport x y w h
        
        processEvents events $ closeOnEscape win
    
        t <- realToFrac . utctDayTime <$> getCurrentTime
    
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)    
    
        forM_ shapes $ \(shape, pos) -> 
            withShape shape $ do
                let model = mkTransformation (axisAngle (V3 1 1 0) t) pos
                uniformM44 uMVP (projection !*! view !*! model)
                drawShape
    
        swapBuffers win
  
