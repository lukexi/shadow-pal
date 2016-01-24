{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.VR.Pal
import Control.Lens
import Halive.Utils
import Control.Monad.Reader
import Control.Monad.State

data Uniforms = Uniforms 
    { uMVP           :: UniformLocation (M44 GLfloat) 
    , uModel         :: UniformLocation (M44 GLfloat) 
    , uLightSpaceMVP :: UniformLocation (M44 GLfloat) 
    , uDiffuse       :: UniformLocation (V4 GLfloat) 
    , uShadowMap     :: UniformLocation GLint
    } deriving Data


main :: IO ()
main = do
    (win, events) <- reacquire 0 $ createWindow "Geometry Test" 1024 768
  
    shader        <- createShaderProgram "app/geo.vert" "app/geo.frag"
    shadowShader  <- createShaderProgram "app/shadowMap.vert" "app/shadowMap.frag"

    shapes       <- makeShapes shader
    -- fix: needless duplication of geo here
    shadowShapes <- makeShapes shadowShader

    (shadowMapFramebuffer, shadowMapTexture) <- createFramebuffer' 1024 1024
    
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
    
    let initialPose = newPose & posPosition .~ (V3 0 1 5)
    void . flip runStateT initialPose . whileWindow win $ do
        processEvents events $ \e -> do
            closeOnEscape win e
        -- applyMouseLook win id
        applyWASD win id

        t <- getNow
        

        glViewport 0 0 1024 1024
        renderShadowMap shadowMapFramebuffer shadowShapes t
        
        pose <- use id
        projM44 <- getWindowProjection win 45 0.1 1000
        let viewM44 = viewMatrixFromPose pose
        (x,y,w,h) <- getWindowViewport win
        glViewport x y w h
        

        
        glActiveTexture GL_TEXTURE0
        bindTexture2D shadowMapTexture
        let projViewM44 = projM44 !*! viewM44
        renderScene projViewM44 shapes t
    
        swapBuffers win

shadowProjView :: M44 GLfloat
shadowProjView = depthProjM44 !*! depthViewM44 
        !*! mkTransformation (axisAngle (V3 0 1 0) 0) (V3 0 0 0)
    where 
        lightInvDir = V3 0.5 2 2
        depthProjM44 = ortho (-10) 10 (-10) 10 (-10) 20
        depthViewM44 = lookAt lightInvDir (V3 0 0 0) (V3 0 1 0)

biasMatrix :: M44 GLfloat
biasMatrix = transpose $
    V4 (V4 0.5 0.0 0.0 0.0)
       (V4 0.0 0.5 0.0 0.0)
       (V4 0.0 0.0 0.5 0.0)
       (V4 0.5 0.5 0.5 1.0)

renderShadowMap :: MonadIO m 
                => Framebuffer
                -> [(Shape Uniforms, V4 GLfloat, t1 -> M44 GLfloat)]
                -> t1
                -> m ()
renderShadowMap shadowFramebuffer shadowShapes t = do
    withFramebuffer' shadowFramebuffer $ do
        renderScene shadowProjView shadowShapes t

renderScene :: (MonadIO m) 
            => M44 GLfloat 
            -> [ (Shape Uniforms, V4 GLfloat, t -> M44 GLfloat) ]
            -> t
            -> m ()
renderScene projViewM44 shapes t = do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    forM_ shapes $ \(shape, color, makeTransform) -> 
        withShape shape $ do
            Uniforms{..} <- asks sUniforms
            let modelM44 = makeTransform t
            uniformM44 uMVP (projViewM44 !*! modelM44)
            uniformM44 uLightSpaceMVP (biasMatrix !*! shadowProjView)
            uniformM44 uModel modelM44
            uniformV4  uDiffuse color
            uniformI   uShadowMap 0
            drawShape



withFramebuffer' :: MonadIO m => Framebuffer -> m a -> m ()
withFramebuffer' (Framebuffer framebuffer) action = do
    glBindFramebuffer GL_FRAMEBUFFER framebuffer
    _ <- action
    glBindFramebuffer GL_FRAMEBUFFER 0


createTexture :: (GLsizei, GLsizei) -> GLenum -> GLint -> GLint -> IO TextureID
createTexture (sizeX, sizeY) texFormat filterType clampType = do
    texID <- overPtr (glGenTextures 1)
    
    glBindTexture   GL_TEXTURE_2D texID
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER filterType
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER filterType
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S clampType
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T clampType
    glTexStorage2D  GL_TEXTURE_2D 1 texFormat sizeX sizeY

    glBindTexture   GL_TEXTURE_2D 0
    
    return (TextureID texID)

bindTexture2D :: MonadIO m => TextureID -> m ()
bindTexture2D (TextureID texID) = glBindTexture GL_TEXTURE_2D texID

createFramebuffer' :: GLsizei -> GLsizei -> IO (Framebuffer, TextureID)
createFramebuffer' sizeX sizeY = do
    framebufferTexture <- createTexture (sizeX, sizeY) 
        GL_DEPTH_COMPONENT32F GL_NEAREST GL_CLAMP_TO_EDGE
  
    framebuffer <- overPtr (glGenFramebuffers 1)
  
    -- Attach the texture as the color buffer
    glBindFramebuffer GL_FRAMEBUFFER framebuffer
    glFramebufferTexture2D GL_FRAMEBUFFER 
        GL_DEPTH_ATTACHMENT GL_TEXTURE_2D 
        (unTextureID framebufferTexture) 0

    glDrawBuffer GL_NONE

    -- glFramebufferTexture2D GL_FRAMEBUFFER 
    --     GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D 
    --     (unTextureID framebufferTexture) 0
  
    -- -- Generate a render buffer for depth
    -- renderbuffer <- overPtr (glGenRenderbuffers 1)
  
    -- -- Configure the depth buffer dimensions to match the color texture
    -- glBindRenderbuffer GL_RENDERBUFFER renderbuffer
    -- glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 sizeX sizeY
    -- glBindRenderbuffer GL_RENDERBUFFER 0
  
    -- -- Attach the render buffer as the depth target
    -- glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER renderbuffer
    
    check <- glCheckFramebufferStatus GL_FRAMEBUFFER
    when (check /= GL_FRAMEBUFFER_COMPLETE) $
        error ("Failed to create framebuffer of size " ++ show (sizeX,sizeY))

    -- Unbind the framebuffer
    glBindFramebuffer GL_FRAMEBUFFER 0
  
    return (Framebuffer framebuffer, framebufferTexture)


makeShapes :: Program -> IO [(Shape Uniforms, V4 GLfloat, GLfloat -> M44 GLfloat)]
makeShapes shader = do
    icoGeo     <- icosahedronGeometry 0.5 5
    icoShape   <- makeShape icoGeo shader
  
    cubeGeo    <- cubeGeometry 1 5
    cubeShape  <- (makeShape cubeGeo shader :: IO (Shape Uniforms))
    
    planeGeo   <- planeGeometry 100 (V3 0 0 1) (V3 0 1 0) 1
    planeShape <- makeShape planeGeo shader
  
    let shapes = [  ( cubeShape
                    , V4 0.5 0.3 1 1
                    , \t -> mkTransformation 
                        (axisAngle (V3 1 1 0) t) (V3 (-1) 1 0)
                    )
                 ,  ( icoShape 
                    , V4 0.2 0.3 0.4 1
                    , \t -> mkTransformation 
                        (axisAngle (V3 1 1 0) t) (V3 1 1 0)
                    )
                 ,  ( planeShape
                    , V4 0 1 0 1
                    , \_t -> mkTransformation 
                        (axisAngle (V3 1 0 0) (-pi/2)) (V3 0 0 0)
                    )
                 ]
    return shapes
