module LLifter.Parser (parser) where

import           Control.Monad.State
import           Data.Array.IO
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char             (ord)
import           Data.Word             (Word8)

import           LLifter.Internal

-- ---------------------------
-- The parser itself
parser :: String -> Game ()
parser file = do
    contents <- liftIO $ liftM BSC.lines $ BSC.readFile file
    let ((size_x,size_y), fld, tramp_assoc, grth, rzr) =
            breakContents (0, 0) defaultFlooding [] defaultGrowth defaultRazor contents
    tbl <- liftIO $ size_x `seq` size_y `seq` empty
                           `seq` newArray ((-1,-1), (size_x+1,size_y+1)) empty
    put $ initGS tbl (size_x,size_y) fld grth rzr
    (tramp_map, target_map) <- fillMap size_y ([], []) contents tbl
    putTramps $ createTramps tramp_map target_map tramp_assoc

fillMap :: Int -> (TrampMap, TargetMap) -> [BS.ByteString]
        -> Table -> Game (TrampMap, TargetMap)
fillMap 0 tramp _ _ = return tramp
fillMap idy tramp (l:ls) tbl = do
    tramp' <- fillMapAux (1, idy) (BS.unpack l) tramp tbl
    let idy' = idy-1
    idy' `seq` fillMap idy' tramp' ls tbl
fillMap _ _ [] _ = error "WAT?"

fillMapAux :: Dimension -> [Word8] -> (TrampMap, TargetMap) -> Table
           -> Game (TrampMap, TargetMap)
fillMapAux point@(idx,idy) (w:ws) tramp@(trmap, tgmap) tbl
    | isRobot w      = do putRobotPos point
                          liftIO $ writeArray tbl point w
                          fillMapAux (idx+1, idy) ws tramp tbl
    | isLambda w     = do putLambdas point
                          liftIO $ writeArray tbl point w
                          fillMapAux (idx+1, idy) ws tramp tbl
    | isCLLift w     = do putExit point
                          liftIO $ writeArray tbl point w
                          fillMapAux (idx+1, idy) ws tramp tbl
    | isTrampoline w = do liftIO $ writeArray tbl point w
                          fillMapAux (idx+1, idy) ws ((w,point):trmap,tgmap) tbl
    | isTarget w     = do liftIO $ writeArray tbl point w
                          fillMapAux (idx+1, idy) ws (trmap,(w,point):tgmap) tbl
    --FIXME: Add foo lambdas (just to know the number of lambdas!)
    | isHighrock w   = do putLambdas (-1, -1)
                          liftIO $ writeArray tbl point w
                          fillMapAux (idx+1, idy) ws tramp tbl
    | otherwise      = do liftIO $ writeArray tbl point w
                          fillMapAux (idx+1, idy) ws tramp tbl
fillMapAux _ [] tramp _ = return tramp

-- ---------------------------
-- Break the contents returning
-- our map dimensions and flooding values
breakContents :: Dimension
              -> Flooding
              -> TrampAssocMap
              -> Growth
              -> Razor
              -> [BS.ByteString]
              -> (Dimension, Flooding, TrampAssocMap, Growth, Razor)
breakContents size@(size_x, size_y) fld tassoc grth rzr (l:ls) =
    let curr_size = BS.length l
        size_x'   = max size_x curr_size
        size_y'   = size_y + 1
    in
    if curr_size /= 0
      then size_x' `seq`
           size_y' `seq`
           breakContents (size_x', size_y') fld tassoc grth rzr ls
      else let (fld', tassoc', grth', rzr') =
                        getMeta fld tassoc grth rzr ls
           in (size, fld', tassoc', grth', rzr')
breakContents size fld tassoc grth rzr [] = (size, fld, tassoc, grth, rzr)

getMeta :: Flooding -> TrampAssocMap -> Growth -> Razor -> [BS.ByteString]
        -> (Flooding, TrampAssocMap, Growth, Razor)
getMeta fld tassoc grth rzr (l:ls)
    | BSC.null l =
        -- EOF (meaningless empty line)
        (fld, tassoc, grth, rzr)
    | head unp == "Trampoline" =
        -- Must be Trampoline meta!
        let ["Trampoline", [name], "targets", [target]] = unp
            name'   = fromIntegral $ ord name
            target' = fromIntegral $ ord target
        in getMeta fld ((name',target'):tassoc) grth rzr ls
    | otherwise =
        -- Must be Flood/Beard meta!
        let [name, valueS] = unp
            value = read valueS
        in
        case name of
            "Water"      -> getMeta (putWater      value fld) tassoc grth rzr ls
            "Flooding"   -> getMeta (putFlooding   value fld) tassoc grth rzr ls
            "Waterproof" -> getMeta (putWaterProof value fld) tassoc grth rzr ls
            "Growth"     -> getMeta fld                       tassoc value  rzr ls
            "Razors"     -> getMeta fld                       tassoc grth value ls
            _            -> error "WAT??"
  where unp = words $ BSC.unpack l
getMeta fld tassoc grth rzr [] = (fld, tassoc, grth, rzr)
