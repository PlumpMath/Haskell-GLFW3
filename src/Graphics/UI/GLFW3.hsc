{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}

module Graphics.UI.GLFW3
  ( -- *   Initialization, termination and version querying
    initialize
  , terminate
  , getGlfwVersion
  , getGlfwVersionString

    -- *   Error handling
  , getError
  , getErrorString
  , setErrorCallback
    --
  , ErrorCallback

    -- *   Video mode information
  , getVideoModes
  , getDesktopMode
    --
  , VideoMode(..)

  , setGamma
  , getGammaRamp
  --, setGammaRamp
    --
  , GammaRamp(..)

    -- *   Window handling
  , openWindow
  , isWindow
  , closeWindow
  , setWindowTitle
  , setWindowSize
  , getWindowSize
  , setWindowPosition
  , getWindowPosition
  , iconifyWindow
  , restoreWindow
  , setWindowUserPointer
  , getWindowUserPointer
  , getWindowParameter
  , windowIsActive
  , windowIsIconified
  , windowIsResizable
  , windowIsHardwareAccelerated
  , windowSupportsStereoRendering
  , getWindowRefreshRate
  , setWindowSizeCallback
  , setWindowCloseCallback
  , setWindowRefreshCallback
  , setWindowFocusCallback
  , setWindowIconifyCallback
    --
  , WindowCloseCallback
  , WindowSizeCallback
  , WindowRefreshCallback
  , WindowFocusCallback
  , WindowIconifyCallback
  , Window
  , DisplayMode(..)
  , DisplayOptions(..)
  , defaultDisplayOptions

    -- *   Event handling
  , pollEvents
  , waitEvents

    -- *   Input handling
  , getInputMode
  , setInputMode
  , InputMode(..)

  , keyIsPressed
  , setKeyCallback
  , setCharCallback
  , Key(..)
  , CharCallback
  , KeyCallback

  , mouseButtonIsPressed
  , getCursorPosition
  , setCursorPosition
  , getScrollOffset
  , setCursorMode
  , getCursorMode
  , setMouseButtonCallback
  , setCursorPositionCallback
  , setCursorEnterCallback
  , setScrollCallback
    --
  , MouseButtonCallback
  , CursorPositionCallback
  , CursorEnterCallback
  , ScrollCallback
  , MouseButton(..)
  , CursorMode(..)

    -- *   Joystick input
  , joystickIsPresent
  , getNumJoystickAxes
  , getNumJoystickButtons
  , getJoystickPosition
  , joystickButtonsArePressed
    --
  , Joystick(..)

    -- *   Clipboard
  , setClipboardString
  , getClipboardString

    -- *   Time
  , getTime
  , setTime

    -- *   OpenGL context
  , makeContextCurrent
  , getCurrentContext
  --, copyContext
  , swapBuffers
  , setSwapInterval
  , extensionIsSupported
  , openGLContextIsForwardCompatible
  , openGLContextIsDebugContext
  , openGLContextIsRobust
  , getOpenGLProfile
  , OpenGLProfile(..)
  ) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad         (when)
import Data.Char             (chr, ord)
import Data.IORef            (IORef, atomicModifyIORef, newIORef)
import Data.Maybe            (fromJust, isJust)
import Data.Version          (Version(..))
import Foreign.C.String      (CString, withCString, peekCString)
import Foreign.C.Types       (CDouble(..), CFloat(..), CInt(..), CULong(..), CUShort(..), CUChar(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr           (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Storable      (Storable(..))
import System.IO.Unsafe      (unsafePerformIO)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

#include <GL/glfw3.h>

foreign import ccall glfwInit                     :: IO CInt
foreign import ccall glfwTerminate                :: IO ()
foreign import ccall glfwGetVersion               :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwGetVersionString         :: IO CString

foreign import ccall glfwGetError                 :: IO CInt
foreign import ccall glfwErrorString              :: CInt -> IO CString
foreign import ccall glfwSetErrorCallback         :: FunPtr GlfwErrorCallback -> IO ()

foreign import ccall glfwGetVideoModes            :: Ptr VideoMode -> CInt -> IO CInt
foreign import ccall glfwGetDesktopMode           :: Ptr VideoMode -> IO ()

foreign import ccall glfwSetGamma                 :: CFloat -> IO ()
foreign import ccall glfwGetGammaRamp             :: IO (Ptr GammaRamp)
foreign import ccall glfwSetGammaRamp             :: Ptr GammaRamp -> IO ()

foreign import ccall glfwOpenWindow               :: CInt -> CInt -> CInt -> CString -> CInt -> IO Window
foreign import ccall glfwOpenWindowHint           :: CInt -> CInt -> IO ()
foreign import ccall glfwIsWindow                 :: Window -> IO CInt
foreign import ccall glfwCloseWindow              :: Window -> IO ()
foreign import ccall glfwSetWindowTitle           :: Window -> CString -> IO ()
foreign import ccall glfwGetWindowSize            :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetWindowSize            :: Window -> CInt -> CInt -> IO ()
foreign import ccall glfwGetWindowPos             :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetWindowPos             :: Window -> CInt -> CInt -> IO ()
foreign import ccall glfwIconifyWindow            :: Window -> IO ()
foreign import ccall glfwRestoreWindow            :: Window -> IO ()
foreign import ccall glfwGetWindowParam           :: Window -> CInt -> IO CInt
foreign import ccall glfwSetWindowUserPointer     :: Window -> Ptr a -> IO ()
foreign import ccall glfwGetWindowUserPointer     :: Window -> IO (Ptr a)
foreign import ccall glfwSetWindowSizeCallback    :: FunPtr GlfwWindowSizeCallback -> IO ()
foreign import ccall glfwSetWindowCloseCallback   :: FunPtr GlfwWindowCloseCallback -> IO ()
foreign import ccall glfwSetWindowRefreshCallback :: FunPtr GlfwWindowRefreshCallback -> IO ()
foreign import ccall glfwSetWindowFocusCallback   :: FunPtr GlfwWindowFocusCallback -> IO ()
foreign import ccall glfwSetWindowIconifyCallback :: FunPtr GlfwWindowIconifyCallback -> IO ()

foreign import ccall glfwPollEvents               :: IO ()
foreign import ccall glfwWaitEvents               :: IO ()

foreign import ccall glfwGetInputMode             :: Window -> CInt -> IO CInt
foreign import ccall glfwSetInputMode             :: Window -> CInt -> CInt -> IO ()
foreign import ccall glfwGetKey                   :: Window -> CInt -> IO CInt
foreign import ccall glfwGetMouseButton           :: Window -> CInt -> IO CInt
foreign import ccall glfwGetCursorPos              :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetCursorPos              :: Window -> CInt -> CInt -> IO ()
foreign import ccall glfwGetScrollOffset          :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetKeyCallback           :: FunPtr GlfwKeyCallback -> IO ()
foreign import ccall glfwSetCharCallback          :: FunPtr GlfwCharCallback -> IO ()
foreign import ccall glfwSetMouseButtonCallback   :: FunPtr GlfwMouseButtonCallback -> IO ()
foreign import ccall glfwSetCursorPosCallback      :: FunPtr GlfwCursorPositionCallback -> IO ()
foreign import ccall glfwSetCursorEnterCallback   :: FunPtr GlfwCursorEnterCallback -> IO ()
foreign import ccall glfwSetScrollCallback        :: FunPtr GlfwScrollCallback -> IO ()

foreign import ccall glfwGetJoystickParam         :: CInt -> CInt -> IO CInt
foreign import ccall glfwGetJoystickPos           :: CInt -> Ptr CFloat -> CInt -> IO CInt
foreign import ccall glfwGetJoystickButtons       :: CInt -> Ptr CUChar -> CInt -> IO CInt

foreign import ccall glfwSetClipboardString       :: Window -> CString -> IO ()
foreign import ccall glfwGetClipboardString       :: Window -> IO CString

foreign import ccall glfwGetTime                  :: IO CDouble
foreign import ccall glfwSetTime                  :: CDouble -> IO ()

foreign import ccall glfwMakeContextCurrent       :: Window -> IO ()
foreign import ccall glfwGetCurrentContext        :: IO Window
foreign import ccall glfwCopyContext              :: Window -> Window -> CULong -> IO ()
foreign import ccall glfwSwapBuffers              :: IO ()
foreign import ccall glfwSwapInterval             :: CInt -> IO ()
foreign import ccall glfwExtensionSupported       :: CString -> IO CInt

type GlfwErrorCallback = CInt -> CString -> IO ()

type GlfwWindowSizeCallback    = Window -> CInt -> CInt -> IO ()
type GlfwWindowCloseCallback   = Window                 -> IO ()
type GlfwWindowRefreshCallback = Window                 -> IO ()
type GlfwWindowFocusCallback   = Window -> CInt         -> IO ()
type GlfwWindowIconifyCallback = Window -> CInt         -> IO ()

type GlfwKeyCallback           = Window -> CInt -> CInt -> IO ()
type GlfwCharCallback          = Window -> CInt         -> IO ()

type GlfwMouseButtonCallback   = Window -> CInt -> CInt -> IO ()
type GlfwCursorPositionCallback = Window -> CInt -> CInt -> IO ()
type GlfwCursorEnterCallback   = Window -> CInt         -> IO ()
type GlfwScrollCallback        = Window -> CDouble -> CDouble -> IO ()


type ErrorCallback = Int -> IO String -> IO ()

type WindowSizeCallback    = Window -> Int -> Int -> IO ()
type WindowCloseCallback   = Window               -> IO ()
type WindowRefreshCallback = Window               -> IO ()
type WindowFocusCallback   = Window -> Bool       -> IO ()
type WindowIconifyCallback = Window -> Bool       -> IO ()

type KeyCallback           = Window -> Key -> Bool              -> IO ()
type CharCallback          = Window -> Char                     -> IO ()

type MouseButtonCallback   = Window -> MouseButton -> Bool -> IO ()
type CursorPositionCallback = Window -> Int -> Int          -> IO ()
type CursorEnterCallback   = Window -> Bool                -> IO ()
type ScrollCallback        = Window -> Double -> Double    -> IO ()


foreign import ccall "wrapper" wrapErrorCallback         :: GlfwErrorCallback         -> IO (FunPtr GlfwErrorCallback)

foreign import ccall "wrapper" wrapWindowSizeCallback    :: GlfwWindowSizeCallback    -> IO (FunPtr GlfwWindowSizeCallback)
foreign import ccall "wrapper" wrapWindowCloseCallback   :: GlfwWindowCloseCallback   -> IO (FunPtr GlfwWindowCloseCallback)
foreign import ccall "wrapper" wrapWindowRefreshCallback :: GlfwWindowRefreshCallback -> IO (FunPtr GlfwWindowRefreshCallback)
foreign import ccall "wrapper" wrapWindowFocusCallback   :: GlfwWindowFocusCallback   -> IO (FunPtr GlfwWindowFocusCallback)
foreign import ccall "wrapper" wrapWindowIconifyCallback :: GlfwWindowIconifyCallback -> IO (FunPtr GlfwWindowIconifyCallback)

foreign import ccall "wrapper" wrapKeyCallback           :: GlfwKeyCallback           -> IO (FunPtr GlfwKeyCallback)
foreign import ccall "wrapper" wrapCharCallback          :: GlfwCharCallback          -> IO (FunPtr GlfwCharCallback)

foreign import ccall "wrapper" wrapMouseButtonCallback    :: GlfwMouseButtonCallback   -> IO (FunPtr GlfwMouseButtonCallback)
foreign import ccall "wrapper" wrapCursorPositionCallback  :: GlfwCursorPositionCallback -> IO (FunPtr GlfwCursorPositionCallback)
foreign import ccall "wrapper" wrapCursorEnterCallback    :: GlfwCursorEnterCallback   -> IO (FunPtr GlfwCursorEnterCallback)
foreign import ccall "wrapper" wrapScrollCallback         :: GlfwScrollCallback        -> IO (FunPtr GlfwScrollCallback)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Initialization, termination version querying

initialize :: IO Bool
initialize =
    fromC `fmap` glfwInit

terminate :: IO ()
terminate =
    glfwTerminate

getGlfwVersion :: IO Version
getGlfwVersion =
    alloca $ \p0 ->
    alloca $ \p1 ->
    alloca $ \p2 -> do
        glfwGetVersion p0 p1 p2
        v0 <- fromC `fmap` peek p0
        v1 <- fromC `fmap` peek p1
        v2 <- fromC `fmap` peek p2
        return $ Version [v0, v1, v2] []

getGlfwVersionString :: IO String
getGlfwVersionString =
    glfwGetVersionString >>= peekCString
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Error handling

getError :: IO Int
getError = 
    fromC `fmap` glfwGetError

getErrorString :: Int -> IO String
getErrorString e =
    glfwErrorString (toC e) >>= peekCString

setErrorCallback :: ErrorCallback -> IO ()
setErrorCallback cb = do
    ccb <- wrapErrorCallback (\er ers -> cb (fromC er) (peekCString ers))
    glfwSetErrorCallback ccb
    storeCallback errorCallback ccb

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Video mode information

getVideoModes :: IO [VideoMode]
getVideoModes =
    allocaArray m $ \ptr -> do
        n <- glfwGetVideoModes ptr (toC m)
        peekArray (fromC n) ptr
  where
    m = 256

getDesktopMode :: IO VideoMode
getDesktopMode =
    alloca $ \ptr -> do
        glfwGetDesktopMode ptr
        peek ptr

-- -- -- -- -- -- -- -- -- --

data VideoMode = VideoMode
  { videoMode_width        :: Int
  , videoMode_height       :: Int
  , videoMode_numRedBits   :: Int
  , videoMode_numGreenBits :: Int
  , videoMode_numBlueBits  :: Int
  } deriving (Eq, Ord, Read, Show)

instance Storable VideoMode where
  sizeOf    _ = #{const sizeof(GLFWvidmode)}
  alignment _ = alignment (undefined :: CInt)

  peek ptr = do
      w <- #{peek GLFWvidmode, width}     ptr :: IO CInt
      h <- #{peek GLFWvidmode, height}    ptr :: IO CInt
      r <- #{peek GLFWvidmode, redBits}   ptr :: IO CInt
      g <- #{peek GLFWvidmode, greenBits} ptr :: IO CInt
      b <- #{peek GLFWvidmode, blueBits}  ptr :: IO CInt
      return VideoMode
        { videoMode_width        = fromC w
        , videoMode_height       = fromC h
        , videoMode_numRedBits   = fromC r
        , videoMode_numGreenBits = fromC g
        , videoMode_numBlueBits  = fromC b
        }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Gamma

setGamma :: Float -> IO ()
setGamma g = glfwSetGamma $ toC g

getGammaRamp :: IO GammaRamp
getGammaRamp =
    glfwGetGammaRamp >>= peek

-- -- -- -- -- -- -- -- -- --
data GammaRamp = GammaRamp
    { red   :: Int
    , green :: Int
    , blue  :: Int
    } deriving (Eq, Ord, Read, Show)

instance Storable GammaRamp where
    sizeOf    _ = #{const GLFW_GAMMA_RAMP_SIZE}
    alignment _ = alignment (undefined :: CUShort)

    peek ptr = do
        r <- #{peek GLFWgammaramp, red}   ptr :: IO CUShort
        g <- #{peek GLFWgammaramp, green} ptr :: IO CUShort
        b <- #{peek GLFWgammaramp, blue}  ptr :: IO CUShort
        return GammaRamp
          { red   = fromC r
          , green = fromC g
          , blue  = fromC b
          }

    poke ptr (GammaRamp r g b) = do
        #{poke GLFWgammaramp, red}   ptr r
        #{poke GLFWgammaramp, green} ptr g
        #{poke GLFWgammaramp, blue}  ptr b

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Window handling
  
openWindow :: DisplayOptions -> IO Window
openWindow displayOptions = do
    let DisplayOptions
          { displayOptions_width                   = _displayOptions_width
          , displayOptions_height                  = _displayOptions_height
          , displayOptions_numRedBits              = _displayOptions_numRedBits
          , displayOptions_numGreenBits            = _displayOptions_numGreenBits
          , displayOptions_numBlueBits             = _displayOptions_numBlueBits
          , displayOptions_numAlphaBits            = _displayOptions_numAlphaBits
          , displayOptions_numDepthBits            = _displayOptions_numDepthBits
          , displayOptions_numStencilBits          = _displayOptions_numStencilBits
          , displayOptions_displayMode             = _displayOptions_displayMode
          , displayOptions_title                   = _displayOptions_title
          , displayOptions_share                   = _displayOptions_share
          , displayOptions_refreshRate             = _displayOptions_refreshRate
          , displayOptions_accumNumRedBits         = _displayOptions_accumNumRedBits
          , displayOptions_accumNumGreenBits       = _displayOptions_accumNumGreenBits
          , displayOptions_accumNumBlueBits        = _displayOptions_accumNumBlueBits
          , displayOptions_accumNumAlphaBits       = _displayOptions_accumNumAlphaBits
          , displayOptions_numAuxiliaryBuffers     = _displayOptions_numAuxiliaryBuffers
          , displayOptions_numFsaaSamples          = _displayOptions_numFsaaSamples
          , displayOptions_windowIsResizable       = _displayOptions_windowIsResizable
          , displayOptions_stereoRendering         = _displayOptions_stereoRendering
          , displayOptions_openGLVersion           = _displayOptions_openGLVersion
          , displayOptions_openGLForwardCompatible = _displayOptions_openGLForwardCompatible
          , displayOptions_openGLDebugContext      = _displayOptions_openGLDebugContext
          , displayOptions_openGLProfile           = _displayOptions_openGLProfile
          , displayOptions_openGLRobustness        = _displayOptions_openGLRobustness
          } = displayOptions

    -- Add hints.
    when (isJust _displayOptions_refreshRate)              $ glfwOpenWindowHint #{const GLFW_REFRESH_RATE}     $ toC (fromJust _displayOptions_refreshRate)
    when (isJust _displayOptions_accumNumRedBits)          $ glfwOpenWindowHint #{const GLFW_ACCUM_RED_BITS}   $ toC (fromJust _displayOptions_accumNumRedBits)
    when (isJust _displayOptions_accumNumGreenBits)        $ glfwOpenWindowHint #{const GLFW_ACCUM_GREEN_BITS} $ toC (fromJust _displayOptions_accumNumGreenBits)
    when (isJust _displayOptions_accumNumBlueBits)         $ glfwOpenWindowHint #{const GLFW_ACCUM_BLUE_BITS}  $ toC (fromJust _displayOptions_accumNumBlueBits)
    when (isJust _displayOptions_accumNumAlphaBits)        $ glfwOpenWindowHint #{const GLFW_ACCUM_ALPHA_BITS} $ toC (fromJust _displayOptions_accumNumAlphaBits)
    when (isJust _displayOptions_numAuxiliaryBuffers)      $ glfwOpenWindowHint #{const GLFW_AUX_BUFFERS}      $ toC (fromJust _displayOptions_numAuxiliaryBuffers)
    when (isJust _displayOptions_numFsaaSamples)           $ glfwOpenWindowHint #{const GLFW_FSAA_SAMPLES}     $ toC (fromJust _displayOptions_numFsaaSamples)

    glfwOpenWindowHint #{const GLFW_WINDOW_RESIZABLE}      $ toC      _displayOptions_windowIsResizable
    glfwOpenWindowHint #{const GLFW_STEREO}                $ toC      _displayOptions_stereoRendering
    glfwOpenWindowHint #{const GLFW_OPENGL_VERSION_MAJOR}  $ toC (fst _displayOptions_openGLVersion)
    glfwOpenWindowHint #{const GLFW_OPENGL_VERSION_MINOR}  $ toC (snd _displayOptions_openGLVersion)
    glfwOpenWindowHint #{const GLFW_OPENGL_FORWARD_COMPAT} $ toC _displayOptions_openGLForwardCompatible
    glfwOpenWindowHint #{const GLFW_OPENGL_DEBUG_CONTEXT}  $ toC _displayOptions_openGLDebugContext
    glfwOpenWindowHint #{const GLFW_OPENGL_PROFILE}        $ toC _displayOptions_openGLProfile
    glfwOpenWindowHint #{const GLFW_OPENGL_ROBUSTNESS}     $ toC _displayOptions_openGLRobustness

    -- Open the window.
    withCString _displayOptions_title (\title -> glfwOpenWindow 
        (toC _displayOptions_width)
        (toC _displayOptions_height)
        (toC _displayOptions_displayMode)
        title
        (toC _displayOptions_share))

isWindow :: Window -> IO Bool
isWindow wd = do
    iw <- glfwIsWindow wd
    return $ fromC iw

closeWindow :: Window -> IO ()
closeWindow wd =
    glfwCloseWindow wd

setWindowTitle :: Window -> String -> IO ()
setWindowTitle wd t =
    withCString t $ glfwSetWindowTitle wd

getWindowSize :: Window -> IO (Int, Int)
getWindowSize wd =
    alloca $ \wp ->
    alloca $ \hp -> do
        glfwGetWindowSize wd wp hp
        w <- peek wp
        h <- peek hp
        return (fromC w, fromC h)

setWindowSize :: Window -> Int -> Int -> IO ()
setWindowSize wd w h =
    glfwSetWindowSize wd (toC w) (toC h)

getWindowPosition :: Window -> IO (Int, Int)
getWindowPosition wd =
    alloca $ \xp ->
    alloca $ \yp -> do
        glfwGetWindowPos wd xp yp
        x <- peek xp
        y <- peek yp
        return (fromC x, fromC y)

setWindowPosition :: Window -> Int -> Int -> IO ()
setWindowPosition wd w h =
    glfwSetWindowPos wd (toC w) (toC h)

iconifyWindow :: Window -> IO ()
iconifyWindow wd =
    glfwIconifyWindow wd

restoreWindow :: Window -> IO ()
restoreWindow wd =
    glfwRestoreWindow wd

setWindowUserPointer :: Window -> Ptr a -> IO ()
setWindowUserPointer wd ptr =
    glfwSetWindowUserPointer wd ptr

getWindowUserPointer :: Window -> IO (Ptr a)
getWindowUserPointer wd =
    glfwGetWindowUserPointer wd

getWindowParameter :: Window -> WindowParameter -> IO Int
getWindowParameter wd wv =
    fromC `fmap` glfwGetWindowParam wd (toC wv)

windowIsActive :: Window -> IO Bool
windowIsActive wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_ACTIVE}

windowIsIconified :: Window -> IO Bool
windowIsIconified wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_ICONIFIED}

windowIsResizable :: Window -> IO Bool
windowIsResizable wd =
    (not . fromC) `fmap` glfwGetWindowParam wd #{const GLFW_WINDOW_RESIZABLE}

windowIsHardwareAccelerated :: Window -> IO Bool
windowIsHardwareAccelerated wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_ACCELERATED}

windowSupportsStereoRendering :: Window -> IO Bool
windowSupportsStereoRendering wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_STEREO}

getWindowRefreshRate :: Window -> IO Int
getWindowRefreshRate wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_REFRESH_RATE}

setWindowSizeCallback :: WindowSizeCallback -> IO ()
setWindowSizeCallback cb = do
    ccb <- wrapWindowSizeCallback (\wd w h -> cb wd (fromC w) (fromC h))
    glfwSetWindowSizeCallback ccb
    storeCallback windowSizeCallback ccb

setWindowCloseCallback :: WindowCloseCallback -> IO ()
setWindowCloseCallback cb = do
    ccb <- wrapWindowCloseCallback (\wd -> cb wd)
    glfwSetWindowCloseCallback ccb
    storeCallback windowCloseCallback ccb

setWindowRefreshCallback :: WindowRefreshCallback -> IO ()
setWindowRefreshCallback cb = do
    ccb <- wrapWindowRefreshCallback (\wd -> cb wd)
    glfwSetWindowRefreshCallback ccb
    storeCallback windowRefreshCallback ccb

setWindowFocusCallback :: WindowFocusCallback -> IO ()
setWindowFocusCallback cb = do
    ccb <- wrapWindowFocusCallback (\wd f -> cb wd (fromC f))
    glfwSetWindowFocusCallback ccb
    storeCallback windowFocusCallback ccb

setWindowIconifyCallback :: WindowIconifyCallback -> IO ()
setWindowIconifyCallback cb = do
    ccb <- wrapWindowIconifyCallback (\wd ic -> cb wd (fromC ic))
    glfwSetWindowIconifyCallback ccb
    storeCallback windowIconifyCallback ccb

-- -- -- -- -- -- -- -- -- --
type Window = Ptr ()

data DisplayMode
  = Windowed
  | Fullscreen
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance C DisplayMode CInt where
  toC dm = case dm of
      Windowed   -> #{const GLFW_WINDOWED}
      Fullscreen -> #{const GLFW_FULLSCREEN}

  fromC i = case i of
      #{const GLFW_WINDOWED}   -> Windowed
      #{const GLFW_FULLSCREEN} -> Fullscreen
      _                        -> makeFromCError "DisplayMode" i

data WindowParameter
  = NumRedBits
  | NumGreenBits
  | NumBlueBits
  | NumAlphaBits
  | NumDepthBits
  | NumStencilBits
  | NumAccumRedBits
  | NumAccumGreenBits
  | NumAccumBlueBits
  | NumAccumAlphaBits
  | NumAuxBuffers
  | NumFsaaSamples
  deriving (Eq, Ord, Bounded, Enum, Read, Show)


instance C WindowParameter CInt where
  toC wn = case wn of
      NumRedBits        -> #const GLFW_RED_BITS
      NumGreenBits      -> #const GLFW_GREEN_BITS
      NumBlueBits       -> #const GLFW_BLUE_BITS
      NumAlphaBits      -> #const GLFW_ALPHA_BITS
      NumDepthBits      -> #const GLFW_DEPTH_BITS
      NumStencilBits    -> #const GLFW_STENCIL_BITS
      NumAccumRedBits   -> #const GLFW_ACCUM_RED_BITS
      NumAccumGreenBits -> #const GLFW_ACCUM_GREEN_BITS
      NumAccumBlueBits  -> #const GLFW_ACCUM_BLUE_BITS
      NumAccumAlphaBits -> #const GLFW_ACCUM_ALPHA_BITS
      NumAuxBuffers     -> #const GLFW_AUX_BUFFERS
      NumFsaaSamples    -> #const GLFW_FSAA_SAMPLES

data DisplayOptions = DisplayOptions
  { displayOptions_width                   :: Int
  , displayOptions_height                  :: Int
  , displayOptions_numRedBits              :: Int
  , displayOptions_numGreenBits            :: Int
  , displayOptions_numBlueBits             :: Int
  , displayOptions_numAlphaBits            :: Int
  , displayOptions_numDepthBits            :: Int
  , displayOptions_numStencilBits          :: Int
  , displayOptions_displayMode             :: DisplayMode
  , displayOptions_title                   :: String
  , displayOptions_share                   :: Int
  , displayOptions_refreshRate             :: Maybe Int
  , displayOptions_accumNumRedBits         :: Maybe Int
  , displayOptions_accumNumGreenBits       :: Maybe Int
  , displayOptions_accumNumBlueBits        :: Maybe Int
  , displayOptions_accumNumAlphaBits       :: Maybe Int
  , displayOptions_numAuxiliaryBuffers     :: Maybe Int
  , displayOptions_numFsaaSamples          :: Maybe Int
  , displayOptions_windowIsResizable       :: Bool
  , displayOptions_stereoRendering         :: Bool
  , displayOptions_openGLVersion           :: (Int, Int)
  , displayOptions_openGLForwardCompatible :: Bool
  , displayOptions_openGLDebugContext      :: Bool
  , displayOptions_openGLProfile           :: OpenGLProfile
  , displayOptions_openGLRobustness        :: OpenGLRobustness

  } deriving (Eq, Ord, Read, Show)

defaultDisplayOptions :: DisplayOptions
defaultDisplayOptions =
    DisplayOptions
      { displayOptions_width                   = 0
      , displayOptions_height                  = 0
      , displayOptions_numRedBits              = 0
      , displayOptions_numGreenBits            = 0
      , displayOptions_numBlueBits             = 0
      , displayOptions_numAlphaBits            = 0
      , displayOptions_numDepthBits            = 0
      , displayOptions_numStencilBits          = 0
      , displayOptions_displayMode             = Windowed
      , displayOptions_title                   = "GLFW3"
      , displayOptions_share                   = 0
      , displayOptions_refreshRate             = Nothing
      , displayOptions_accumNumRedBits         = Nothing
      , displayOptions_accumNumGreenBits       = Nothing
      , displayOptions_accumNumBlueBits        = Nothing
      , displayOptions_accumNumAlphaBits       = Nothing
      , displayOptions_numAuxiliaryBuffers     = Nothing
      , displayOptions_numFsaaSamples          = Nothing
      , displayOptions_windowIsResizable       = True
      , displayOptions_stereoRendering         = False
      , displayOptions_openGLVersion           = (3,2)
      , displayOptions_openGLForwardCompatible = True
      , displayOptions_openGLDebugContext      = False
      , displayOptions_openGLProfile           = DefaultProfile
      , displayOptions_openGLRobustness        = NoRobustness
      }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Event handling

pollEvents :: IO ()
pollEvents =
    glfwPollEvents

waitEvents :: IO ()
waitEvents =
    glfwWaitEvents

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Input handling

getInputMode :: Window -> InputMode -> IO Bool
getInputMode wd im = do 
    m <- glfwGetInputMode wd (toC im)
    return $ fromC m

setInputMode :: Window -> InputMode -> Bool -> IO ()
setInputMode wd im b =
    glfwSetInputMode wd (toC im) (toC b)

-- -- -- -- -- -- -- -- -- --

data InputMode 
  = StickyKeys
  | StickyMouseButtons
  | SystemKeys
  | KeyRepeat
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance C InputMode CInt where
  toC im = case im of
    StickyKeys -> #{const GLFW_STICKY_KEYS}
    StickyMouseButtons -> #{const GLFW_STICKY_MOUSE_BUTTONS}
    SystemKeys -> #{const GLFW_SYSTEM_KEYS}
    KeyRepeat -> #{const GLFW_KEY_REPEAT}

  fromC i = case i of
    #{const GLFW_STICKY_KEYS}          -> StickyKeys
    #{const GLFW_STICKY_MOUSE_BUTTONS} -> StickyMouseButtons
    #{const GLFW_SYSTEM_KEYS}          -> SystemKeys
    #{const GLFW_KEY_REPEAT}           -> KeyRepeat
    _                                  -> makeFromCError "InputMode" i

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Keyboard

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed wd k =
    fromC `fmap` glfwGetKey wd (toC k)

setKeyCallback :: KeyCallback -> IO ()
setKeyCallback cb = do
    ccb <- wrapKeyCallback (\wd k b -> cb wd (fromC k) (fromC b))
    glfwSetKeyCallback ccb
    storeCallback keyCallback ccb

setCharCallback :: CharCallback -> IO ()
setCharCallback cb = do
    ccb <- wrapCharCallback (\wd c -> cb wd (fromC c))
    glfwSetCharCallback ccb
    storeCallback charCallback ccb

-- -- -- -- -- -- -- -- -- --

data Key
-- printable keys
  = KeySpace
  | KeyApostrophe
  | KeyComma
  | KeyMinus
  | KeyPeriod
  | KeySlash
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeySemicolon
  | KeyEqual
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | KeyLeftBracket
  | KeyBackslash
  | KeyRightBracket
  | KeyGraveAccent
  | KeyWorld1
  | KeyWorld2
-- Function keys
  | KeyEscape
  | KeyEnter
  | KeyTab
  | KeyBackspace
  | KeyInsert
  | KeyDelete
  | KeyRight
  | KeyLeft
  | KeyDown
  | KeyUp
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd
  | KeyCapsLock
  | KeyScrollLock
  | KeyNumLock
  | KeyPrintScreen
  | KeyPause
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyF25
  | KeyKeypad0
  | KeyKeypad1
  | KeyKeypad2
  | KeyKeypad3
  | KeyKeypad4
  | KeyKeypad5
  | KeyKeypad6
  | KeyKeypad7
  | KeyKeypad8
  | KeyKeypad9
  | KeyKeypadDecimal
  | KeyKeypadDivide
  | KeyKeypadMultiply
  | KeyKeypadSubtract
  | KeyKeypadAdd
  | KeyKeypadEnter
  | KeyKeypadEqual
  | KeyLeftShift
  | KeyLeftControl
  | KeyLeftAlt
  | KeyLeftSuper
  | KeyRightShift
  | KeyRightControl
  | KeyRightAlt
  | KeyRightSuper
  | KeyMenu
  deriving (Eq, Ord, Read, Show)

instance C Key CInt where
  toC k = case k of
      KeySpace              -> #{const GLFW_KEY_SPACE}                
      KeyApostrophe         -> #{const GLFW_KEY_APOSTROPHE}           
      KeyComma              -> #{const GLFW_KEY_COMMA}                
      KeyMinus              -> #{const GLFW_KEY_MINUS}                
      KeyPeriod             -> #{const GLFW_KEY_PERIOD}               
      KeySlash              -> #{const GLFW_KEY_SLASH}                
      Key0                  -> #{const GLFW_KEY_0}                    
      Key1                  -> #{const GLFW_KEY_1}                    
      Key2                  -> #{const GLFW_KEY_2}                   
      Key3                  -> #{const GLFW_KEY_3}                   
      Key4                  -> #{const GLFW_KEY_4}                   
      Key5                  -> #{const GLFW_KEY_5}                   
      Key6                  -> #{const GLFW_KEY_6}                   
      Key7                  -> #{const GLFW_KEY_7}                   
      Key8                  -> #{const GLFW_KEY_8}                   
      Key9                  -> #{const GLFW_KEY_9}                   
      KeySemicolon          -> #{const GLFW_KEY_SEMICOLON}           
      KeyEqual              -> #{const GLFW_KEY_EQUAL}               
      KeyA                  -> #{const GLFW_KEY_A}                   
      KeyB                  -> #{const GLFW_KEY_B}                   
      KeyC                  -> #{const GLFW_KEY_C}                   
      KeyD                  -> #{const GLFW_KEY_D}                   
      KeyE                  -> #{const GLFW_KEY_E}                   
      KeyF                  -> #{const GLFW_KEY_F}                   
      KeyG                  -> #{const GLFW_KEY_G}                   
      KeyH                  -> #{const GLFW_KEY_H}                   
      KeyI                  -> #{const GLFW_KEY_I}                   
      KeyJ                  -> #{const GLFW_KEY_J}                   
      KeyK                  -> #{const GLFW_KEY_K}                   
      KeyL                  -> #{const GLFW_KEY_L}                   
      KeyM                  -> #{const GLFW_KEY_M}                   
      KeyN                  -> #{const GLFW_KEY_N}                   
      KeyO                  -> #{const GLFW_KEY_O}                   
      KeyP                  -> #{const GLFW_KEY_P}                   
      KeyQ                  -> #{const GLFW_KEY_Q}                   
      KeyR                  -> #{const GLFW_KEY_R}                   
      KeyS                  -> #{const GLFW_KEY_S}                   
      KeyT                  -> #{const GLFW_KEY_T}                   
      KeyU                  -> #{const GLFW_KEY_U}                   
      KeyV                  -> #{const GLFW_KEY_V}                   
      KeyW                  -> #{const GLFW_KEY_W}                   
      KeyX                  -> #{const GLFW_KEY_X}                   
      KeyY                  -> #{const GLFW_KEY_Y}                   
      KeyZ                  -> #{const GLFW_KEY_Z}                   
      KeyLeftBracket        -> #{const GLFW_KEY_LEFT_BRACKET}         
      KeyBackslash          -> #{const GLFW_KEY_BACKSLASH}           
      KeyRightBracket       -> #{const GLFW_KEY_RIGHT_BRACKET}       
      KeyGraveAccent        -> #{const GLFW_KEY_GRAVE_ACCENT}        
      KeyWorld1             -> #{const GLFW_KEY_WORLD_1}             
      KeyWorld2             -> #{const GLFW_KEY_WORLD_2}             
      KeyEscape             -> #{const GLFW_KEY_ESCAPE}                    
      KeyEnter              -> #{const GLFW_KEY_ENTER}               
      KeyTab                -> #{const GLFW_KEY_TAB}                 
      KeyBackspace          -> #{const GLFW_KEY_BACKSPACE}           
      KeyInsert             -> #{const GLFW_KEY_INSERT}              
      KeyDelete             -> #{const GLFW_KEY_DELETE}              
      KeyRight              -> #{const GLFW_KEY_RIGHT}               
      KeyLeft               -> #{const GLFW_KEY_LEFT}                
      KeyDown               -> #{const GLFW_KEY_DOWN}                
      KeyUp                 -> #{const GLFW_KEY_UP}                  
      KeyPageUp             -> #{const GLFW_KEY_PAGE_UP}             
      KeyPageDown           -> #{const GLFW_KEY_PAGE_DOWN}           
      KeyHome               -> #{const GLFW_KEY_HOME}                
      KeyEnd                -> #{const GLFW_KEY_END}                 
      KeyCapsLock           -> #{const GLFW_KEY_CAPS_LOCK}            
      KeyScrollLock         -> #{const GLFW_KEY_SCROLL_LOCK}         
      KeyNumLock            -> #{const GLFW_KEY_NUM_LOCK}            
      KeyPrintScreen        -> #{const GLFW_KEY_PRINT_SCREEN}        
      KeyPause              -> #{const GLFW_KEY_PAUSE}               
      KeyF1                 -> #{const GLFW_KEY_F1}                  
      KeyF2                 -> #{const GLFW_KEY_F2}                  
      KeyF3                 -> #{const GLFW_KEY_F3}                  
      KeyF4                 -> #{const GLFW_KEY_F4}                  
      KeyF5                 -> #{const GLFW_KEY_F5}                  
      KeyF6                 -> #{const GLFW_KEY_F6}                  
      KeyF7                 -> #{const GLFW_KEY_F7}                  
      KeyF8                 -> #{const GLFW_KEY_F8}                  
      KeyF9                 -> #{const GLFW_KEY_F9}                  
      KeyF10                -> #{const GLFW_KEY_F10}                 
      KeyF11                -> #{const GLFW_KEY_F11}                 
      KeyF12                -> #{const GLFW_KEY_F12}                 
      KeyF13                -> #{const GLFW_KEY_F13}                 
      KeyF14                -> #{const GLFW_KEY_F14}                 
      KeyF15                -> #{const GLFW_KEY_F15}                 
      KeyF16                -> #{const GLFW_KEY_F16}                 
      KeyF17                -> #{const GLFW_KEY_F17}                 
      KeyF18                -> #{const GLFW_KEY_F18}                 
      KeyF19                -> #{const GLFW_KEY_F19}                 
      KeyF20                -> #{const GLFW_KEY_F20}                 
      KeyF21                -> #{const GLFW_KEY_F21}                 
      KeyF22                -> #{const GLFW_KEY_F22}                 
      KeyF23                -> #{const GLFW_KEY_F23}                 
      KeyF24                -> #{const GLFW_KEY_F24}                 
      KeyF25                -> #{const GLFW_KEY_F25}                 
      KeyKeypad0            -> #{const GLFW_KEY_KP_0}                
      KeyKeypad1            -> #{const GLFW_KEY_KP_1}                
      KeyKeypad2            -> #{const GLFW_KEY_KP_2}                
      KeyKeypad3            -> #{const GLFW_KEY_KP_3}                
      KeyKeypad4            -> #{const GLFW_KEY_KP_4}                
      KeyKeypad5            -> #{const GLFW_KEY_KP_5}                
      KeyKeypad6            -> #{const GLFW_KEY_KP_6}                
      KeyKeypad7            -> #{const GLFW_KEY_KP_7}                
      KeyKeypad8            -> #{const GLFW_KEY_KP_8}                
      KeyKeypad9            -> #{const GLFW_KEY_KP_9}                
      KeyKeypadDecimal      -> #{const GLFW_KEY_KP_DECIMAL}           
      KeyKeypadDivide       -> #{const GLFW_KEY_KP_DIVIDE}            
      KeyKeypadMultiply     -> #{const GLFW_KEY_KP_MULTIPLY}         
      KeyKeypadSubtract     -> #{const GLFW_KEY_KP_SUBTRACT}         
      KeyKeypadAdd          -> #{const GLFW_KEY_KP_ADD}              
      KeyKeypadEnter        -> #{const GLFW_KEY_KP_ENTER}            
      KeyKeypadEqual        -> #{const GLFW_KEY_KP_EQUAL}            
      KeyLeftShift          -> #{const GLFW_KEY_LEFT_SHIFT}          
      KeyLeftControl        -> #{const GLFW_KEY_LEFT_CONTROL}        
      KeyLeftAlt            -> #{const GLFW_KEY_LEFT_ALT}            
      KeyLeftSuper          -> #{const GLFW_KEY_LEFT_SUPER}          
      KeyRightShift         -> #{const GLFW_KEY_RIGHT_SHIFT}         
      KeyRightControl       -> #{const GLFW_KEY_RIGHT_CONTROL}       
      KeyRightAlt           -> #{const GLFW_KEY_RIGHT_ALT}           
      KeyRightSuper         -> #{const GLFW_KEY_RIGHT_SUPER}         
      KeyMenu               -> #{const GLFW_KEY_MENU}
                                       
  fromC i =
      case i of
        #{const GLFW_KEY_SPACE        } -> KeySpace              
        #{const GLFW_KEY_APOSTROPHE   } -> KeyApostrophe         
        #{const GLFW_KEY_COMMA        } -> KeyComma              
        #{const GLFW_KEY_MINUS        } -> KeyMinus              
        #{const GLFW_KEY_PERIOD       } -> KeyPeriod             
        #{const GLFW_KEY_SLASH        } -> KeySlash              
        #{const GLFW_KEY_0            } -> Key0                  
        #{const GLFW_KEY_1            } -> Key1                  
        #{const GLFW_KEY_2            } -> Key2                  
        #{const GLFW_KEY_3            } -> Key3                  
        #{const GLFW_KEY_4            } -> Key4                  
        #{const GLFW_KEY_5            } -> Key5                  
        #{const GLFW_KEY_6            } -> Key6                  
        #{const GLFW_KEY_7            } -> Key7                  
        #{const GLFW_KEY_8            } -> Key8                  
        #{const GLFW_KEY_9            } -> Key9                  
        #{const GLFW_KEY_SEMICOLON    } -> KeySemicolon          
        #{const GLFW_KEY_EQUAL        } -> KeyEqual              
        #{const GLFW_KEY_A            } -> KeyA                  
        #{const GLFW_KEY_B            } -> KeyB                  
        #{const GLFW_KEY_C            } -> KeyC                  
        #{const GLFW_KEY_D            } -> KeyD                  
        #{const GLFW_KEY_E            } -> KeyE                  
        #{const GLFW_KEY_F            } -> KeyF                  
        #{const GLFW_KEY_G            } -> KeyG                  
        #{const GLFW_KEY_H            } -> KeyH                  
        #{const GLFW_KEY_I            } -> KeyI                  
        #{const GLFW_KEY_J            } -> KeyJ                  
        #{const GLFW_KEY_K            } -> KeyK                  
        #{const GLFW_KEY_L            } -> KeyL                  
        #{const GLFW_KEY_M            } -> KeyM                  
        #{const GLFW_KEY_N            } -> KeyN                  
        #{const GLFW_KEY_O            } -> KeyO                  
        #{const GLFW_KEY_P            } -> KeyP                  
        #{const GLFW_KEY_Q            } -> KeyQ                  
        #{const GLFW_KEY_R            } -> KeyR                  
        #{const GLFW_KEY_S            } -> KeyS                  
        #{const GLFW_KEY_T            } -> KeyT                  
        #{const GLFW_KEY_U            } -> KeyU                  
        #{const GLFW_KEY_V            } -> KeyV                  
        #{const GLFW_KEY_W            } -> KeyW                  
        #{const GLFW_KEY_X            } -> KeyX                  
        #{const GLFW_KEY_Y            } -> KeyY                  
        #{const GLFW_KEY_Z            } -> KeyZ                  
        #{const GLFW_KEY_LEFT_BRACKET } -> KeyLeftBracket        
        #{const GLFW_KEY_BACKSLASH    } -> KeyBackslash          
        #{const GLFW_KEY_RIGHT_BRACKET} -> KeyRightBracket       
        #{const GLFW_KEY_GRAVE_ACCENT } -> KeyGraveAccent        
        #{const GLFW_KEY_WORLD_1      } -> KeyWorld1             
        #{const GLFW_KEY_WORLD_2      } -> KeyWorld2             
        #{const GLFW_KEY_ESCAPE       } -> KeyEscape                   
        #{const GLFW_KEY_ENTER        } -> KeyEnter              
        #{const GLFW_KEY_TAB          } -> KeyTab                
        #{const GLFW_KEY_BACKSPACE    } -> KeyBackspace          
        #{const GLFW_KEY_INSERT       } -> KeyInsert             
        #{const GLFW_KEY_DELETE       } -> KeyDelete             
        #{const GLFW_KEY_RIGHT        } -> KeyRight              
        #{const GLFW_KEY_LEFT         } -> KeyLeft               
        #{const GLFW_KEY_DOWN         } -> KeyDown               
        #{const GLFW_KEY_UP           } -> KeyUp                 
        #{const GLFW_KEY_PAGE_UP      } -> KeyPageUp             
        #{const GLFW_KEY_PAGE_DOWN    } -> KeyPageDown           
        #{const GLFW_KEY_HOME         } -> KeyHome               
        #{const GLFW_KEY_END          } -> KeyEnd                
        #{const GLFW_KEY_CAPS_LOCK    } -> KeyCapsLock           
        #{const GLFW_KEY_SCROLL_LOCK  } -> KeyScrollLock         
        #{const GLFW_KEY_NUM_LOCK     } -> KeyNumLock            
        #{const GLFW_KEY_PRINT_SCREEN } -> KeyPrintScreen        
        #{const GLFW_KEY_PAUSE        } -> KeyPause              
        #{const GLFW_KEY_F1           } -> KeyF1                 
        #{const GLFW_KEY_F2           } -> KeyF2                 
        #{const GLFW_KEY_F3           } -> KeyF3                 
        #{const GLFW_KEY_F4           } -> KeyF4                 
        #{const GLFW_KEY_F5           } -> KeyF5                 
        #{const GLFW_KEY_F6           } -> KeyF6                 
        #{const GLFW_KEY_F7           } -> KeyF7                 
        #{const GLFW_KEY_F8           } -> KeyF8                 
        #{const GLFW_KEY_F9           } -> KeyF9                 
        #{const GLFW_KEY_F10          } -> KeyF10                
        #{const GLFW_KEY_F11          } -> KeyF11                
        #{const GLFW_KEY_F12          } -> KeyF12                
        #{const GLFW_KEY_F13          } -> KeyF13                
        #{const GLFW_KEY_F14          } -> KeyF14                
        #{const GLFW_KEY_F15          } -> KeyF15                
        #{const GLFW_KEY_F16          } -> KeyF16                
        #{const GLFW_KEY_F17          } -> KeyF17                
        #{const GLFW_KEY_F18          } -> KeyF18                
        #{const GLFW_KEY_F19          } -> KeyF19                
        #{const GLFW_KEY_F20          } -> KeyF20                
        #{const GLFW_KEY_F21          } -> KeyF21                
        #{const GLFW_KEY_F22          } -> KeyF22                
        #{const GLFW_KEY_F23          } -> KeyF23                
        #{const GLFW_KEY_F24          } -> KeyF24                
        #{const GLFW_KEY_F25          } -> KeyF25                
        #{const GLFW_KEY_KP_0         } -> KeyKeypad0            
        #{const GLFW_KEY_KP_1         } -> KeyKeypad1            
        #{const GLFW_KEY_KP_2         } -> KeyKeypad2            
        #{const GLFW_KEY_KP_3         } -> KeyKeypad3            
        #{const GLFW_KEY_KP_4         } -> KeyKeypad4            
        #{const GLFW_KEY_KP_5         } -> KeyKeypad5            
        #{const GLFW_KEY_KP_6         } -> KeyKeypad6            
        #{const GLFW_KEY_KP_7         } -> KeyKeypad7            
        #{const GLFW_KEY_KP_8         } -> KeyKeypad8            
        #{const GLFW_KEY_KP_9         } -> KeyKeypad9            
        #{const GLFW_KEY_KP_DECIMAL   } -> KeyKeypadDecimal      
        #{const GLFW_KEY_KP_DIVIDE    } -> KeyKeypadDivide       
        #{const GLFW_KEY_KP_MULTIPLY  } -> KeyKeypadMultiply     
        #{const GLFW_KEY_KP_SUBTRACT  } -> KeyKeypadSubtract     
        #{const GLFW_KEY_KP_ADD       } -> KeyKeypadAdd          
        #{const GLFW_KEY_KP_ENTER     } -> KeyKeypadEnter        
        #{const GLFW_KEY_KP_EQUAL     } -> KeyKeypadEqual        
        #{const GLFW_KEY_LEFT_SHIFT   } -> KeyLeftShift          
        #{const GLFW_KEY_LEFT_CONTROL } -> KeyLeftControl        
        #{const GLFW_KEY_LEFT_ALT     } -> KeyLeftAlt            
        #{const GLFW_KEY_LEFT_SUPER   } -> KeyLeftSuper          
        #{const GLFW_KEY_RIGHT_SHIFT  } -> KeyRightShift         
        #{const GLFW_KEY_RIGHT_CONTROL} -> KeyRightControl       
        #{const GLFW_KEY_RIGHT_ALT    } -> KeyRightAlt           
        #{const GLFW_KEY_RIGHT_SUPER  } -> KeyRightSuper         
        #{const GLFW_KEY_MENU         } -> KeyMenu               
        _                               -> makeFromCError "Key" i

-- Mouse

mouseButtonIsPressed :: Window -> MouseButton -> IO Bool
mouseButtonIsPressed wd mb =
    fromC `fmap` glfwGetMouseButton wd (toC mb)

getCursorPosition :: Window -> IO (Int, Int)
getCursorPosition wd =
    alloca $ \px ->
    alloca $ \py -> do
        glfwGetCursorPos wd px py
        x <- peek px
        y <- peek py
        return (fromC x, fromC y)

setCursorPosition :: Window -> Int -> Int -> IO ()
setCursorPosition wd x y =
    glfwSetCursorPos wd (toC x) (toC y)

getScrollOffset :: Window -> IO (Int, Int)
getScrollOffset wd = 
    alloca $ \ox ->
    alloca $ \oy -> do
        glfwGetScrollOffset wd ox oy
        x <- peek ox
        y <- peek oy
        return (fromC x, fromC y)

setCursorMode :: Window -> CursorMode -> IO ()
setCursorMode wd cm = glfwSetInputMode wd #{const GLFW_CURSOR_MODE} (toC cm)

getCursorMode :: Window -> IO CursorMode
getCursorMode wd = do
    m <- glfwGetInputMode wd #{const GLFW_CURSOR_MODE} 
    return $ fromC m

setMouseButtonCallback :: MouseButtonCallback -> IO ()
setMouseButtonCallback cb = do
    ccb <- wrapMouseButtonCallback (\wd b p -> cb wd (fromC b) (fromC p))
    glfwSetMouseButtonCallback ccb
    storeCallback mouseButtonCallback ccb

setCursorPositionCallback :: CursorPositionCallback -> IO ()
setCursorPositionCallback cb = do
    ccb <- wrapCursorPositionCallback (\wd x y -> cb wd (fromC x) (fromC y))
    glfwSetCursorPosCallback ccb
    storeCallback cursorPositionCallback ccb

setCursorEnterCallback :: CursorEnterCallback -> IO ()
setCursorEnterCallback cb = do
    ccb <- wrapCursorEnterCallback (\wd e -> cb wd (fromC e))
    glfwSetCursorEnterCallback ccb
    storeCallback cursorEnterCallback ccb

setScrollCallback :: ScrollCallback -> IO ()
setScrollCallback cb = do
    ccb <- wrapScrollCallback (\wd x y -> cb wd (fromC x) (fromC y))
    glfwSetScrollCallback ccb
    storeCallback scrollCallback ccb

-- -- -- -- -- -- -- -- -- --

data MouseButton
  = MouseButton0 | MouseButton1 | MouseButton2 | MouseButton3
  | MouseButton4 | MouseButton5 | MouseButton6 | MouseButton7
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance C MouseButton CInt where
  toC mb = case mb of
      MouseButton0 -> #const GLFW_MOUSE_BUTTON_1
      MouseButton1 -> #const GLFW_MOUSE_BUTTON_2
      MouseButton2 -> #const GLFW_MOUSE_BUTTON_3
      MouseButton3 -> #const GLFW_MOUSE_BUTTON_4
      MouseButton4 -> #const GLFW_MOUSE_BUTTON_5
      MouseButton5 -> #const GLFW_MOUSE_BUTTON_6
      MouseButton6 -> #const GLFW_MOUSE_BUTTON_7
      MouseButton7 -> #const GLFW_MOUSE_BUTTON_8

  fromC i = case i of
      #{const GLFW_MOUSE_BUTTON_1} -> MouseButton0
      #{const GLFW_MOUSE_BUTTON_2} -> MouseButton1
      #{const GLFW_MOUSE_BUTTON_3} -> MouseButton2
      #{const GLFW_MOUSE_BUTTON_4} -> MouseButton3
      #{const GLFW_MOUSE_BUTTON_5} -> MouseButton4
      #{const GLFW_MOUSE_BUTTON_6} -> MouseButton5
      #{const GLFW_MOUSE_BUTTON_7} -> MouseButton6
      #{const GLFW_MOUSE_BUTTON_8} -> MouseButton7
      _                            -> makeFromCError "MouseButton" i

data CursorMode
  = Normal | Hidden | Captured
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance C CursorMode CInt where
  toC cb = case cb of
      Normal   -> #const GLFW_CURSOR_NORMAL
      Hidden   -> #const GLFW_CURSOR_HIDDEN
      Captured -> #const GLFW_CURSOR_CAPTURED

  fromC i = case i of
      #{const GLFW_CURSOR_NORMAL}   -> Normal
      #{const GLFW_CURSOR_HIDDEN}   -> Hidden
      #{const GLFW_CURSOR_CAPTURED} -> Captured
      _                             -> makeFromCError "CursorMode" i

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Joystick

joystickIsPresent :: Joystick -> IO Bool
joystickIsPresent j =
    fromC `fmap` glfwGetJoystickParam (toC j) #{const GLFW_PRESENT}

getNumJoystickAxes :: Joystick -> IO Int
getNumJoystickAxes j =
    fromC `fmap` glfwGetJoystickParam (toC j) #{const GLFW_AXES}

getNumJoystickButtons :: Joystick -> IO Int
getNumJoystickButtons j =
    fromC `fmap` glfwGetJoystickParam (toC j) #{const GLFW_BUTTONS}

getJoystickPosition :: Joystick -> Int -> IO [Float]
getJoystickPosition j m =
    if m < 1
      then return []
      else allocaArray m $ \ptr -> do
               n <- fromC `fmap` glfwGetJoystickPos (toC j) ptr (toC m)
               a <- peekArray n ptr
               return $ map fromC a

joystickButtonsArePressed :: Joystick -> Int -> IO [Bool]
joystickButtonsArePressed j m =
    if m < 1
      then return []
      else allocaArray m $ \ptr -> do
               n <- fromC `fmap` glfwGetJoystickButtons (toC j) ptr (toC m)
               a <- peekArray n ptr :: IO [CUChar]
               return $ map ((#{const GLFW_PRESS} ==) . fromIntegral) a

-- -- -- -- -- -- -- -- -- --

data Joystick
  = Joystick0  | Joystick1  | Joystick2  | Joystick3
  | Joystick4  | Joystick5  | Joystick6  | Joystick7
  | Joystick8  | Joystick9  | Joystick10 | Joystick11
  | Joystick12 | Joystick13 | Joystick14 | Joystick15
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance C Joystick CInt where
  toC j = case j of
      Joystick0  -> #{const GLFW_JOYSTICK_1}
      Joystick1  -> #{const GLFW_JOYSTICK_2}
      Joystick2  -> #{const GLFW_JOYSTICK_3}
      Joystick3  -> #{const GLFW_JOYSTICK_4}
      Joystick4  -> #{const GLFW_JOYSTICK_5}
      Joystick5  -> #{const GLFW_JOYSTICK_6}
      Joystick6  -> #{const GLFW_JOYSTICK_7}
      Joystick7  -> #{const GLFW_JOYSTICK_8}
      Joystick8  -> #{const GLFW_JOYSTICK_9}
      Joystick9  -> #{const GLFW_JOYSTICK_10}
      Joystick10 -> #{const GLFW_JOYSTICK_11}
      Joystick11 -> #{const GLFW_JOYSTICK_12}
      Joystick12 -> #{const GLFW_JOYSTICK_13}
      Joystick13 -> #{const GLFW_JOYSTICK_14}
      Joystick14 -> #{const GLFW_JOYSTICK_15}
      Joystick15 -> #{const GLFW_JOYSTICK_16}

  fromC i = case i of
      #{const GLFW_JOYSTICK_1 } -> Joystick0
      #{const GLFW_JOYSTICK_2 } -> Joystick1
      #{const GLFW_JOYSTICK_3 } -> Joystick2
      #{const GLFW_JOYSTICK_4 } -> Joystick3
      #{const GLFW_JOYSTICK_5 } -> Joystick4
      #{const GLFW_JOYSTICK_6 } -> Joystick5
      #{const GLFW_JOYSTICK_7 } -> Joystick6
      #{const GLFW_JOYSTICK_8 } -> Joystick7
      #{const GLFW_JOYSTICK_9 } -> Joystick8
      #{const GLFW_JOYSTICK_10} -> Joystick9
      #{const GLFW_JOYSTICK_11} -> Joystick10
      #{const GLFW_JOYSTICK_12} -> Joystick11
      #{const GLFW_JOYSTICK_13} -> Joystick12
      #{const GLFW_JOYSTICK_14} -> Joystick13
      #{const GLFW_JOYSTICK_15} -> Joystick14
      #{const GLFW_JOYSTICK_16} -> Joystick15
      _                         -> makeFromCError "Joystick" i

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Clipboard

setClipboardString :: Window -> String -> IO ()
setClipboardString wd cp =
    withCString cp (\cps -> glfwSetClipboardString wd cps)

getClipboardString :: Window -> IO String
getClipboardString wd =
    glfwGetClipboardString wd >>= peekCString

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Time

getTime :: IO Double
getTime =
    realToFrac `fmap` glfwGetTime

setTime :: Double -> IO ()
setTime =
    glfwSetTime . realToFrac

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- OpenGL context

makeContextCurrent :: Window -> IO ()
makeContextCurrent wd =
    glfwMakeContextCurrent wd

getCurrentContext :: IO Window
getCurrentContext =
    glfwGetCurrentContext

-- glfwCopyContext expects a GL_ datatype, so including it would create a dependency on OpenGL
--copyContext :: Window -> Window -> glEnum -> IO ()
--copyContext =
--    glfwCopyContext

swapBuffers :: IO ()
swapBuffers = 
    glfwSwapBuffers

setSwapInterval :: Int -> IO ()
setSwapInterval =
    glfwSwapInterval . toC

extensionIsSupported :: String -> IO Bool
extensionIsSupported ext = do
    fromC `fmap` withCString ext glfwExtensionSupported

openGLContextIsForwardCompatible :: Window -> IO Bool
openGLContextIsForwardCompatible wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_OPENGL_FORWARD_COMPAT}

openGLContextIsDebugContext :: Window -> IO Bool
openGLContextIsDebugContext wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_OPENGL_DEBUG_CONTEXT}

openGLContextIsRobust :: Window -> IO OpenGLRobustness
openGLContextIsRobust wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_OPENGL_ROBUSTNESS}

getOpenGLProfile :: Window -> IO OpenGLProfile
getOpenGLProfile wd =
    fromC `fmap` glfwGetWindowParam wd #{const GLFW_OPENGL_PROFILE}

-- -- -- -- -- -- -- -- --

data OpenGLProfile
  = DefaultProfile
  | CoreProfile
  | CompatibilityProfile
  | ES2Profile
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance C OpenGLProfile CInt where
  toC op = case op of
      DefaultProfile       -> #{const GLFW_OPENGL_NO_PROFILE}
      CoreProfile          -> #{const GLFW_OPENGL_CORE_PROFILE}
      CompatibilityProfile -> #{const GLFW_OPENGL_COMPAT_PROFILE}
      ES2Profile           -> #{const GLFW_OPENGL_ES2_PROFILE}
  fromC i = case i of
      #{const GLFW_OPENGL_NO_PROFILE}     -> DefaultProfile
      #{const GLFW_OPENGL_CORE_PROFILE}   -> CoreProfile
      #{const GLFW_OPENGL_COMPAT_PROFILE} -> CompatibilityProfile
      #{const GLFW_OPENGL_ES2_PROFILE}    -> ES2Profile
      _                                   -> makeFromCError "OpenGLProfile" i

data OpenGLRobustness
 = NoRobustness
 | NoResetNotification
 | LoseContextOnReset
 deriving (Eq, Ord, Bounded, Enum, Read, Show)

instance C OpenGLRobustness CInt where
    toC rb = case rb of
        NoRobustness        -> #{const GLFW_OPENGL_NO_ROBUSTNESS}
        NoResetNotification -> #{const GLFW_OPENGL_NO_RESET_NOTIFICATION}
        LoseContextOnReset  -> #{const GLFW_OPENGL_LOSE_CONTEXT_ON_RESET}

    fromC i = case i of
        #{const GLFW_OPENGL_NO_ROBUSTNESS}         -> NoRobustness
        #{const GLFW_OPENGL_NO_RESET_NOTIFICATION} -> NoResetNotification
        #{const GLFW_OPENGL_LOSE_CONTEXT_ON_RESET} -> LoseContextOnReset
        _                                          -> makeFromCError "OpenGLRobustness" i

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class C h c where
  toC   :: h -> c
  fromC :: c -> h

  toC   = undefined
  fromC = undefined

makeFromCError :: (Show c) => String -> c -> a
makeFromCError s c = error (s ++ " fromC: no match for " ++ show c)

-- -- -- -- -- -- -- -- -- --

instance C Bool CInt where
  toC False = #{const GL_FALSE}
  toC True  = #{const GL_TRUE}

  fromC #{const GL_FALSE} = False
  fromC #{const GL_TRUE}  = True
  fromC i                 = makeFromCError "Bool" i

-- -- -- -- -- -- -- -- -- --

instance C Char CInt where
  toC   = fromIntegral . ord
  fromC = chr . fromIntegral

-- -- -- -- -- -- -- -- -- --

instance C Float CFloat where
  toC   = realToFrac
  fromC = realToFrac

-- -- -- -- -- -- -- -- -- --

instance C Double CDouble where 
  toC   = realToFrac
  fromC = realToFrac

-- -- -- -- -- -- -- -- -- --

instance C Int CInt where
  toC   = fromIntegral
  fromC = fromIntegral
  
-- -- -- -- -- -- -- -- -- --

instance C Int CUShort where
  toC   = fromIntegral
  fromC = fromIntegral

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

errorCallback :: IORef (Maybe (FunPtr GlfwErrorCallback))

windowSizeCallback    :: IORef (Maybe (FunPtr GlfwWindowSizeCallback))
windowCloseCallback   :: IORef (Maybe (FunPtr GlfwWindowCloseCallback))
windowRefreshCallback :: IORef (Maybe (FunPtr GlfwWindowRefreshCallback))
windowFocusCallback   :: IORef (Maybe (FunPtr GlfwWindowFocusCallback))
windowIconifyCallback :: IORef (Maybe (FunPtr GlfwWindowIconifyCallback))

keyCallback           :: IORef (Maybe (FunPtr GlfwKeyCallback))
charCallback          :: IORef (Maybe (FunPtr GlfwCharCallback))

mouseButtonCallback   :: IORef (Maybe (FunPtr GlfwMouseButtonCallback))
cursorPositionCallback :: IORef (Maybe (FunPtr GlfwCursorPositionCallback))
cursorEnterCallback   :: IORef (Maybe (FunPtr GlfwCursorEnterCallback))
scrollCallback        :: IORef (Maybe (FunPtr GlfwScrollCallback))

errorCallback         = unsafePerformIO (newIORef Nothing)
{-# NOINLINE errorCallback #-}

windowSizeCallback    = unsafePerformIO (newIORef Nothing)
{-# NOINLINE windowSizeCallback #-}
windowCloseCallback   = unsafePerformIO (newIORef Nothing)
{-# NOINLINE windowCloseCallback #-}
windowRefreshCallback = unsafePerformIO (newIORef Nothing)
{-# NOINLINE windowRefreshCallback #-}
windowFocusCallback   = unsafePerformIO (newIORef Nothing)
{-# NOINLINE windowFocusCallback #-}
windowIconifyCallback = unsafePerformIO (newIORef Nothing)
{-# NOINLINE windowIconifyCallback #-}

keyCallback           = unsafePerformIO (newIORef Nothing)
{-# NOINLINE keyCallback #-}
charCallback          = unsafePerformIO (newIORef Nothing)
{-# NOINLINE charCallback #-}

mouseButtonCallback   = unsafePerformIO (newIORef Nothing)
{-# NOINLINE mouseButtonCallback #-}
cursorPositionCallback = unsafePerformIO (newIORef Nothing)
{-# NOINLINE cursorPositionCallback #-}
cursorEnterCallback   = unsafePerformIO (newIORef Nothing)
{-# NOINLINE cursorEnterCallback #-}
scrollCallback        = unsafePerformIO (newIORef Nothing)
{-# NOINLINE scrollCallback #-}

storeCallback :: IORef (Maybe (FunPtr a)) -> FunPtr a -> IO ()
storeCallback ior cb =
    atomicModifyIORef ior (\mcb -> (Just cb, mcb)) >>= maybe (return ()) freeHaskellFunPtr
