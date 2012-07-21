module LLifter.OpenGL (openGLmain) where

import LLifter.Internal
import LLifter.Parser

import Data.StateVar
import Data.Array.IO
import Graphics.Rendering.OpenGL hiding (translate, scale)
import Graphics.UI.GLUT hiding (translate, scale)
import Graphics.DrawingCombinators
 
openGLmain :: String -> IO ()
openGLmain file = do 
    (_progname, _) <- getArgsAndInitialize
    state <- parserIO file
    _ <- createWindow "Lambda Lifter"
    sprites <- readSprites
    let reshape (Size windowWidth windowHeight) = do
        (_, (boardWidth, boardHeight)) <- getBounds $ table state
        let windowAspectRatio :: Double = fromIntegral windowWidth / fromIntegral windowHeight
        let boardAspectRatio :: Double = fromIntegral boardWidth / fromIntegral boardHeight
        let (width', height') = if boardAspectRatio > windowAspectRatio
                                then (fromIntegral windowWidth, fromIntegral windowWidth * (1/boardAspectRatio))
                                else (fromIntegral windowHeight * boardAspectRatio, fromIntegral windowHeight)
        let (width, height) = (ceiling width', ceiling height')
        viewport $= ((Position (fromIntegral ((windowWidth - width) `div` 2)) (fromIntegral ((windowHeight - height) `div` 2))), (Size width height))
    reshapeCallback $= Just reshape
    let makeImage = do
        (_, (width, height)) <- getBounds $ table state
        elements <- getAssocs $ table state
        let makeTransform x y = translate (-1, -1) `compose` scale 2 2 `compose` scale (1/(fromIntegral width - 1)) (1/(fromIntegral height - 1))
                                `compose` translate (x-0.5, y-0.5) `compose` scale (1/2) (1/2)
        return $ foldl (\image ((x, y), cell) ->
                    if isEmpty cell then image
                    else image `mappend` (makeTransform (fromIntegral x) (fromIntegral y) %% (sprite $ (findSpriteFromCell cell) sprites))
                 ) mempty elements
    let display = do
        clear [ColorBuffer]
        image <- makeImage
        render $ image
        flush
    displayCallback $= display
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

