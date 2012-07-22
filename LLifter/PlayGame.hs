module LLifter.PlayGame (runGame, playGameIO) where

import           Control.Monad             (when, unless, void)
import           Control.Monad.Loops       (iterateWhile)
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Array.IO
import           Data.Maybe

import           LLifter.Internal
import           LLifter.Parser

-- ---------------------------
-- Let's play
runGame :: String -> IO ()
runGame file = evalStateT (startGame file) undefined

startGame :: String -> Game ()
startGame file = void (parser file >> printState Playing >>
                         iterateWhile (Playing ==) playGame)

playGame :: Game GameConditions
playGame = doMove >>= updateTable >>= printState
    where doMove = iterateWhile (not . validMove) (liftIO getChar) >>=
                   moveRobot . read . flip (:) []

playGameIO :: GameState -> Movement -> IO (GameConditions, GameState)
playGameIO gs m = evalStateT (put gs >> moveRobot m >>= updateTable >>= (gets . (,))) undefined

-- ---------------------------
-- Move our Robot
moveRobot :: Movement -> Game Movement
moveRobot MWait  = return MWait
moveRobot MAbort = return MAbort
moveRobot movement = do gs <- get
                        let tbl = table gs
                            m_from = robotPos gs
                            m_to = m_from `moveTo` movement
                            bs = bounds gs
                        if m_to `inBounds` bs
                          then do c_to  <- liftIO $ readArray tbl m_to
                                  moved <- moveRobotAux movement tbl m_from (m_to,c_to)
                                  if moved
                                    then return movement
                                    else return MWait
                          else return MWait

moveRobotAux :: Movement -> Table -> Dimension -> (Dimension, Cell) -> Game Bool
moveRobotAux movement tbl m_from@(x,y) (m_to, c_to)
    | isEmpty c_to || isEarth c_to = normalRobotMove >> return True
    | isTrampoline c_to            = do liftIO $ writeArray tbl m_from empty
                                        liftIO $ writeArray tbl m_to empty
                                        GS{tramps = ts} <- get
                                        let jump_to = fromJust $ lookup m_to ts
                                        liftIO $ writeArray tbl jump_to robot
                                        putRobotPos jump_to
                                        -- Erase all trampolines to that target!
                                        liftIO $ mapM_ (
                                            \pos -> writeArray tbl pos empty
                                          ) (getSources jump_to ts [])
                                        return True
    | isLambda c_to                = do normalRobotMove
                                        collectLambda m_to
                                        return True
    | isOLLift c_to                = do liftIO $ writeArray tbl m_from empty
                                        putRobotPos m_to
                                        return True
    | isRazor c_to                 = do normalRobotMove
                                        incRazors
                                        return True
    | movement == MRight && isRocky c_to = do
                                        let (x_to,y_to) = m_to
                                            m_next = (x_to+1,y_to)
                                        temp <- liftIO $ readArray tbl m_next
                                        if isEmpty temp
                                          then robotMovesRock m_next >> return True
                                          else return False
    | movement == MLeft && isRocky c_to  = do
                                        let (x_to,y_to) = m_to
                                            m_next = (x_to-1,y_to)
                                        temp <- liftIO $ readArray tbl m_next
                                        if isEmpty temp
                                          then robotMovesRock m_next >> return True
                                          else return False
    | movement == MRazor           = do gs <- get
                                        let availableRazors = razors gs
                                        when (availableRazors>0) $ razeThemUp >> decRazors
                                        putRobotPos m_to
                                        return True
    | otherwise                    = return False
  where razeThemUp :: Game ()
        razeThemUp = do -- Raze them all! }:-] (sudo make me readable)
                      elems <- liftIO $ mapM (readArray tbl)
                               [ (x+dx,y+dy) | dx<-[-1..1], dy<-[-1..1] ]
                      let elems' = zip elems [ (x+dx,y+dy) | dx<-[-1..1], dy<-[-1..1] ]
                          elems'' = filter (\(e,_) -> isBeard e) elems'
                      liftIO $ mapM_ (\(_, pos) -> writeArray tbl pos empty) elems''
        getSources :: Dimension -> Tramps -> [Dimension] -> [Dimension]
        getSources targetPos ((spos, tpos):more) acc
          | targetPos == tpos = getSources targetPos more (spos:acc)
          | otherwise         = getSources targetPos more acc
        getSources _ [] acc   = acc
        normalRobotMove :: Game ()
        normalRobotMove = do liftIO $ writeArray tbl m_from empty
                             liftIO $ writeArray tbl m_to   robot
                             putRobotPos m_to
        robotMovesRock  :: Dimension -> Game ()
        robotMovesRock m_next = do liftIO $ writeArray tbl m_next rock
                                   normalRobotMove

-- ---------------------------
-- Update our Table
updateTable :: Movement -> Game GameConditions
updateTable MAbort = return Abort
updateTable _ = do gs <- get
                   let t          = table gs
                       (size_x, size_y) = bounds gs
                       gth        = growth gs
                       trn        = turn gs
                       must_grow  = (trn `mod` gth) == 0
                       rob        = robotPos gs
                       fld        = flood gs
                   -- create a new array
                   dt <- liftIO $ newArray ((-1,-1), (size_x+1,size_y+1)) empty
                   liftIO $ mapM_ (updateTableAux dt t must_grow)
                            [ (x,y) | y<-[1..size_y], x<-[1..size_x] ]
                   putTable dt
                   -- Update water info
                   putFlood (updateWater trn fld)
                   incMovesUnderWaterIfNeeded rob
                   -- Update turn and return
                   incrTurn
                   isDrawn <- checkIsDrawn
                   liftIO $ checkConditions rob dt t isDrawn
  where incMovesUnderWaterIfNeeded :: Dimension -> Game ()
        incMovesUnderWaterIfNeeded (_,y) = do
          gs <- get
          let water' = (water . flood) gs
          if y <= water'
            then incMovesUnderWater
            else resetMovesUnderWater
        checkIsDrawn :: Game Bool
        checkIsDrawn = do GS{movesUnderWater=muw, flood=f} <- get
                          return (muw > waterproof f)

updateTableAux :: Table -> Table -> Bool -> Dimension -> IO ()
updateTableAux new_t old_t must_grow point@(x,y) = do
    elems <- mapM (readArray old_t)
             [ (x+dx,y+dy) | dx<-[-1..1], dy<-[-2..1] ]
    updateTableAux' new_t must_grow elems point

updateTableAux' :: Table -> Bool -> [Cell] -> Dimension -> IO ()
updateTableAux' new_t must_grow elems (x,y)
    | isRocky e && isEmpty ed  = do writeArray new_t (x,y) empty
                                    moveRocky (x,y-1) edd
    | isRocky e && isRocky ed && isEmpty er && isEmpty erd
                               = do writeArray new_t (x,y) empty
                                    moveRocky (x+1,y-1) erdd
    | isRocky e && isRocky ed && ((not . isEmpty) er || (not . isEmpty) erd)
                && isEmpty el && isEmpty eld
                               = do writeArray new_t (x,y) empty
                                    moveRocky (x-1,y-1) eldd
    | isRocky e && isLambda ed && isEmpty er && isEmpty erd
                               = do writeArray new_t (x,y) empty
                                    moveRocky (x+1,y-1) erdd
    | isBeard e && must_grow   = mapM_ (\(it,i) -> when (isEmpty it || isBeard it) $
                                                   writeArray new_t i beard) fringe
    | otherwise                = do ru <- recentlyUpdated (x,y)
                                    unless ru $ writeArray new_t (x,y) e
  where allElems :: [(Cell, Dimension)]
        allElems@[ (eldd, _), (eld, _), (el, _), _
                 , (edd,  _), (ed,  _), (e,  _), _
                 , (erdd, _), (erd, _), (er, _), _ ] =
          zip elems [ (x+dx,y+dy) | dx<-[-1..1], dy<-[-2..1] ]
        -- Fringe is allElems excluding two floors down
        fringe :: [(Cell, Dimension)]
        fringe = filter (\(_,(_,yy)) -> yy>y-2) allElems
        moveRocky :: Dimension -> Cell -> IO ()
        moveRocky (tx, ty) old_elem =
          if isHighrock e && ((not . isEmpty) old_elem || ty-2 == 1)
                          && (not . isRobot) old_elem
            then writeArray new_t (tx,ty) lambda
            else writeArray new_t (tx,ty) e
        recentlyUpdated :: Dimension -> IO Bool
        recentlyUpdated (nx,ny) =  do curElem <- readArray new_t (nx, ny)
                                      return $ (not . isEmpty) curElem

checkConditions :: Dimension -> Table -> Table -> Bool -> IO GameConditions
checkConditions (rx,ry) new_tbl old_tbl isDrawn = do
    new_up  <- readArray new_tbl (rx,ry+1)
    old_up  <- readArray old_tbl (rx,ry+1)
    old_cur <- readArray old_tbl (rx,ry)
    checkConditions' new_up old_up old_cur isDrawn

checkConditions' :: Cell -> Cell -> Cell -> Bool -> IO GameConditions
checkConditions' new_up old_up old_cur isDrawn
    | isDrawn                                  = return Losing
    | isOLLift old_cur                         = return Winning
    | isRocky new_up && (not . isRocky) old_up = return Losing
    | otherwise                                = return Playing

-- ---------------------------
-- Print out State
printState :: GameConditions -> Game GameConditions
printState gd = get >>= \gs ->
    liftIO $ do putStr "\ESC[H\ESC[2J"
                printTable (bounds gs) (table gs) (waterLevel gs)
                putStrLn ""
                putStrLn $ "Robot is in: "      ++ (show . robotPos) gs
                putStrLn ""
                putStrLn $ "Water level: "      ++ (show . waterLevel) gs
                putStrLn $ "Waterproof : "      ++ (show . waterproof . flood) gs
                putStrLn $ "Underwater moves: " ++ (show . movesUnderWater) gs
                putStrLn ""
                putStrLn $ "Growth     : "      ++ (show . growth) gs
                putStrLn $ "Beard-growth in : " ++ (show . beardRemaining) gs
                putStrLn $ "Razors     : "      ++ (show . razors) gs
                putStrLn "--------------------"
                putStrLn $ "Turn       : "      ++ (show . pred . turn) gs
                putStrLn $ "Score      : "      ++ show (getScore gd gs)
    >> return gd
  where waterLevel :: GameState -> Int
        waterLevel = water . flood
        beardRemaining :: GameState -> Int
        beardRemaining gs = let (gr, tn) = (growth gs, turn gs) in
                            (gr-1) - (tn-1) `mod` gr

printTable :: Dimension -> Table -> Int -> IO ()
printTable (size_x,size_y) tbl waterLevel = mapM_ ppAll [size_y,size_y-1..1]
  where isFlooded  :: Int -> Bool
        isFlooded  = (waterLevel >=)
        ppLine     :: Int -> Int -> IO ()
        ppLine y x = readArray tbl (x,y) >>= putStr . showCell (isFlooded y)
        ppAll      :: Int -> IO ()
        ppAll y    = mapM_ (ppLine y) [1..size_x] >> putStrLn ""
