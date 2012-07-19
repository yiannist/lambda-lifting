module LLifter.Internal where

import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Array.IO
import           Data.Char
import qualified Data.List                 as List
import           Data.Maybe
import           Data.Word

-- ---------------------------
-- Table
type Table = IOUArray Dimension Word8

type Dimension = (Int, Int)

-- ---------------------------
-- Game Type
type Game = StateT GameState IO

data GameState = GS {
    table           :: Table
  , bounds          :: Dimension
  , flood           :: Flooding
  , robotPos        :: Dimension   -- ^ Robot's current position
  , lambdas         :: [Dimension] -- ^ Available lambdas
  , exit            :: Dimension
  , gathered        :: Int         -- ^ Lambdas gathered
  , tramps          :: Tramps      -- ^ Map of Trampolines -> Targets
  , growth          :: Growth
  , razors          :: Razor
  , turn            :: Int         -- ^ Current turn
  , movesUnderWater :: Int         -- ^ Underwater moves
}

initGS :: Table -> Dimension -> Flooding -> Growth -> Razor -> GameState
initGS tbl bnds fld grth rzr =
    GS tbl bnds fld (1,1) [] (1,1) 0 [] grth rzr 1 0

getScore :: GameConditions -> GameState -> Int
getScore gc GS{turn = t, gathered = g} =
    let mult = case gc of
                    Playing -> 25
                    Abort   -> 50
                    Winning -> 75
                    Losing  -> 25
    in mult*g - t + 1

putTable :: Table -> Game ()
putTable new_table = do
    gs <- get
    put gs{table = new_table}

putBounds :: Dimension -> Game ()
putBounds bs = do
    gs <- get
    put gs{bounds = bs}

putRobotPos :: Dimension -> Game ()
putRobotPos rp = do
    gs <- get
    put gs{robotPos = rp}

putLambdas :: Dimension -> Game ()
putLambdas l = do
    gs@(GS{lambdas = ls}) <- get
    put gs{lambdas = l : ls}

collectLambda :: Dimension -> Game ()
collectLambda dim = do
    gs@(GS{gathered = l, lambdas = ls}) <- get
    let ls' = List.delete dim ls
    if null ls'
       then openLambda gs >> put gs{gathered = l+1, lambdas = ls'}
       else put gs{lambdas = ls', gathered = l+1}

openLambda :: GameState -> Game ()
openLambda GS{table = tbl, exit = ext} =
    liftIO $ writeArray tbl ext ollift

putExit :: Dimension -> Game ()
putExit e = do
    gs <- get
    put gs{exit = e}

putFlood :: Flooding -> Game ()
putFlood fl = do
    gs <- get
    put gs{flood = fl}

putTramps :: Tramps -> Game ()
putTramps t = do
    gs <- get
    put gs{tramps = t}

incrTurn :: Game ()
incrTurn = do
    gs@(GS{turn = trn}) <- get
    put gs{turn = succ trn}

incMovesUnderWater :: Game ()
incMovesUnderWater = do
    gs@GS{movesUnderWater = muw} <- get
    put gs{movesUnderWater = succ muw}

resetMovesUnderWater :: Game ()
resetMovesUnderWater = do
    gs <- get
    put gs{movesUnderWater = 0}

incRazors :: Game ()
incRazors = do
    gs@GS{razors = r} <- get
    put gs{razors = succ r}

decRazors :: Game ()
decRazors = do
    gs@GS{razors = r} <- get
    put gs{razors = pred r}

-- ---------------------------
-- Cells
type Cell = Word8

robot, rock, cllift, earth, lambda, wall, ollift, empty,
    beard, razor, highrock :: Cell
robot    = fromIntegral (ord 'R')
rock     = fromIntegral (ord '*')
cllift   = fromIntegral (ord 'L')
earth    = fromIntegral (ord '.')
lambda   = fromIntegral (ord '\\')
wall     = fromIntegral (ord '#')
ollift   = fromIntegral (ord 'O')
empty    = fromIntegral (ord ' ')
beard    = fromIntegral (ord 'W')
razor    = fromIntegral (ord '!')
highrock = fromIntegral (ord '@')

isRobot, isRock, isCLLift, isEarth, isLambda, isWall, isOLLift, isEmpty,
  isTarget, isTrampoline, isBeard, isRazor, isHighrock :: Cell -> Bool
isRobot      = (==) robot
isRock       = (==) rock
isCLLift     = (==) cllift
isEarth      = (==) earth
isLambda     = (==) lambda
isWall       = (==) wall
isOLLift     = (==) ollift
isEmpty      = (==) empty
isTrampoline = flip elem [(fromIntegral . ord) 'A'..(fromIntegral . ord) 'I']
isTarget     = flip elem [(fromIntegral . ord) '1'..(fromIntegral . ord) '9']
isBeard      = (==) beard
isRazor      = (==) razor
isHighrock   = (==) highrock

-- "Rocky" is either rock or highrock
isRocky :: Cell -> Bool
isRocky e = isRock e || isHighrock e

showCell :: Bool -> Cell -> String
showCell isFlooded cell
    | isRobot  cell = withColor 36
    | isWall   cell = withColor 34
    | isBeard  cell = withColor 34
    | isRock   cell = withColor 35
    | isLambda cell = withColor 33
    | isEarth  cell = withColor 32
    | isRazor  cell = withColor 32
    | isCLLift cell = withColor 31
    | isOLLift cell = withColor 31
    | isTrampoline cell = withColor 31
    | isTarget cell     = withColor 31
    | otherwise     = withColor 37
  where bg = if isFlooded then show (44::Int) else show (49::Int)
        c = (chr . fromIntegral) cell
        withColor :: Int -> String
        withColor fg = "\x1b[" ++ show fg ++ ";" ++ bg ++ ";1m" ++ [c] ++ "\x1b[0m"


-- ---------------------------
-- Game Conditions
data GameConditions
    = Winning
    | Abort
    | Losing
    | Playing
    deriving (Eq)

-- ---------------------------
-- Movements
data Movement
    = MLeft
    | MRight
    | MUp
    | MDown
    | MWait
    | MAbort
    | MRazor
    deriving (Eq)

instance Show Movement where
    show MLeft  = "M"
    show MRight = "R"
    show MUp    = "U"
    show MDown  = "D"
    show MWait  = "W"
    show MAbort = "A"
    show MRazor = "S"

instance Read Movement where
    -- readsPrec is the main function for parsing input
    readsPrec _ value =
        -- We pass tryParse a list of pairs.  Each pair has a string
        -- and the desired return value.  tryParse will try to match
        -- the input to one of these strings.
        tryParse [("i", MUp), ("k", MDown), ("j", MLeft), ("l", MRight)
                 ,("a", MAbort), ("w", MWait), ("s", MRazor)]
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                  -- Compare the start of the string to be parsed to the
                  -- text we are looking for.
                  if take (length attempt) value == attempt
                     -- If we have a match, return the result and the
                     -- remaining input
                     then [(result, drop (length attempt) value)]
                     -- If we don't have a match, try the next pair
                     -- in the list of attempts.
                     else tryParse xs

validMove :: Char -> Bool
validMove = flip elem "ikjlaws"

moveTo :: Dimension -> Movement -> Dimension
moveTo (x, y) m
    | m == MLeft  = (x-1, y)
    | m == MRight = (x+1, y)
    | m == MUp    = (x, y+1)
    | m == MDown  = (x, y-1)
    | m == MWait  = (x, y)
    | m == MAbort = (x, y)
    | m == MRazor = (x, y)
    | otherwise   = error "What??"

inBounds :: Dimension -> Dimension -> Bool
inBounds (x, y) (lim_x, lim_y) =
    (1 <= x && x <= lim_x) && (1 <= y && y <= lim_y)

-- ---------------------------
-- Flooding state
data Flooding = F {
    water       :: Int
  , flooding    :: Int
  , waterproof  :: Int
}

defaultFlooding :: Flooding
defaultFlooding = F 0 0 10

putWater, putFlooding, putWaterProof :: Int -> Flooding -> Flooding
putWater      w  f = f{water = w}
putFlooding   fl f = f{flooding = fl}
putWaterProof w  f = f{waterproof = w}

updateWater :: Int -> Flooding -> Flooding
updateWater trn fld@(F w f _) =
    if f > 0 && trn `mod` f == 0
       then fld{water = w+1}
       else fld

-- ---------------------------
-- Fucking Trampolines
type Trampoline    = Word8
type Target        = Word8
type TrampMap      = [(Trampoline, Dimension)]
type TargetMap     = [(Target, Dimension)]
type TrampAssocMap = [(Trampoline, Target)]
type Tramps = [(Dimension, Dimension)]

createTramps :: TrampMap -> TargetMap -> TrampAssocMap -> Tramps
createTramps trmap tgmap = foldl f []
    where f :: Tramps -> (Trampoline, Target) -> Tramps
          f trmps (tr, tg) =
            let dim1 = fromJust $ lookup tr trmap
                dim2 = fromJust $ lookup tg tgmap
            in (dim1,dim2):trmps

-- ---------------------------
-- Growths
type Growth = Int
type Razor  = Int

defaultGrowth, defaultRazor :: Int
defaultGrowth = 25
defaultRazor  = 0
