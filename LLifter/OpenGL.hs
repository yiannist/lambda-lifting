module LLifter.OpenGL (runGame) where

import LLifter.Internal
import LLifter.PlayGame hiding (runGame)
import LLifter.Parser

import Data.StateVar
import Data.Array.IO
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Color, translate, scale)
import Graphics.UI.GLUT hiding (Color, translate, scale, initState)
import Graphics.DrawingCombinators
 
runGame :: String -> IO ()
runGame file = do 
    (_progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initState <- parserIO file
    _ <- createWindow "Lambda Lifter"
    sprites <- readSprites
    gameState <- newIORef initState
    gameCondition <- newIORef Playing
    let reshape (Size windowWidth windowHeight) = do
        (_, (boardWidth, boardHeight)) <- getBounds $ table initState
        let windowAspectRatio :: Double = fromIntegral windowWidth / fromIntegral windowHeight
        let boardAspectRatio :: Double = fromIntegral boardWidth / fromIntegral boardHeight
        let (width', height') = if boardAspectRatio > windowAspectRatio
                                then (fromIntegral windowWidth, fromIntegral windowWidth * (1/boardAspectRatio))
                                else (fromIntegral windowHeight * boardAspectRatio, fromIntegral windowHeight)
        let (width, height) = (ceiling width', ceiling height')
        viewport $= ((Position (fromIntegral ((windowWidth - width) `div` 2)) (fromIntegral ((windowHeight - height) `div` 2))), (Size width height))
    reshapeCallback $= Just reshape
    let makeImage = do
        state <- get gameState
        (_, (width, height)) <- getBounds $ table state
        elements <- getAssocs $ table state
        let makeTransform x y = translate (-1, -1) `compose` scale 2 2 `compose` scale (1/(fromIntegral width - 1)) (1/(fromIntegral height - 1))
                                `compose` translate (x-0.5, y-0.5) `compose` scale (1/2) (1/2)
        let boardImage = foldl (\image ((x, y), cell) ->
                    if isEmpty cell then image
                    else image `mappend` (makeTransform (fromIntegral x) (fromIntegral y) %% (sprite $ (findSpriteFromCell cell) sprites))
                 ) mempty elements
        let waterLevel = water $ flood state
        let waterImage = translate (0, -2*(fromIntegral height - 1 - fromIntegral waterLevel)/(fromIntegral height - 1)) %%
                         Color 0 1 1 0.5 `tint` convexPoly [(1, 1), (1, -1), (-1, -1), (-1, 1)]
        return $ waterImage `mappend` boardImage
    let display = do
        clear [ColorBuffer]
        image <- makeImage
        render $ image
        swapBuffers
    displayCallback $= display
    let keyboard (Char char) Up _ _ = do
            condition <- get gameCondition
            if condition == Playing && validMove char then do
                state <- get gameState
                (condition', state') <- playGameIO state (read [char])
                gameState $= state'
                gameCondition $= condition'
                postRedisplay Nothing
            else return ()
        keyboard _ _ _ _ = return ()
    keyboardMouseCallback $= Just keyboard
    mainLoop


data Sprites = Sprites {
      robotSP    :: Sprite
    , rockSP     :: Sprite
    , clliftSP   :: Sprite
    , earthSP    :: Sprite
    , lambdaSP   :: Sprite
    , wallSP     :: Sprite
    , olliftSP   :: Sprite
    , targetSP   :: Sprite
    , trampSP    :: Sprite
    , beardSP    :: Sprite
    , razorSP    :: Sprite
    , highrockSP :: Sprite
}

findSpriteFromCell :: Cell -> Sprites -> Sprite
findSpriteFromCell c | isRobot      c = robotSP
findSpriteFromCell c | isRock       c = rockSP
findSpriteFromCell c | isCLLift     c = clliftSP
findSpriteFromCell c | isEarth      c = earthSP
findSpriteFromCell c | isLambda     c = lambdaSP
findSpriteFromCell c | isWall       c = wallSP
findSpriteFromCell c | isOLLift     c = olliftSP
findSpriteFromCell c | isTarget     c = targetSP
findSpriteFromCell c | isTrampoline c = trampSP
findSpriteFromCell c | isBeard      c = beardSP
findSpriteFromCell c | isRazor      c = razorSP
findSpriteFromCell c | isHighrock   c = highrockSP
findSpriteFromCell c                  = error ("No sprite for cell: " ++ show c)

readSprites :: IO Sprites
readSprites = do
    robotSprite    <- openSprite "bitmaps/robot.bmp"
    rockSprite     <- openSprite "bitmaps/rock.bmp"
    clliftSprite   <- openSprite "bitmaps/cllift.bmp"
    earthSprite    <- openSprite "bitmaps/earth.bmp"
    lambdaSprite   <- openSprite "bitmaps/lambda.bmp"
    wallSprite     <- openSprite "bitmaps/wall.bmp"
    olliftSprite   <- openSprite "bitmaps/ollift.bmp"
    targetSprite   <- openSprite "bitmaps/target.bmp"
    trampSprite    <- openSprite "bitmaps/tramp.bmp"
    beardSprite    <- openSprite "bitmaps/beard.bmp"
    razorSprite    <- openSprite "bitmaps/razor.bmp"
    highrockSprite <- openSprite "bitmaps/highrock.bmp"
    return $ Sprites { robotSP = robotSprite, rockSP = rockSprite, clliftSP = clliftSprite, earthSP = earthSprite, lambdaSP = lambdaSprite,
                       wallSP = wallSprite, olliftSP = olliftSprite, targetSP = targetSprite, trampSP = trampSprite, beardSP = beardSprite,
                       razorSP = razorSprite, highrockSP = highrockSprite }

