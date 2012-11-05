module System(
  System(..),
  makeSystem,
  addRow,
  solve
             )
  where

import Rhs
import Data.List (sortBy)

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MGV
import qualified Data.Vector.Unboxed as UV

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

type Row = IntSet

get :: Vector a -> Int -> a
get = (V.!)
{-# INLINE get #-}
-- TODO This most likely leaks space
set :: Vector a -> Int -> a -> Vector a
set v i e = (V.//) v [(i, e)]
{-# INLINE set #-}

degree :: Row -> Int
degree = IS.size

-- 'sub' takes 2 rows and an integer
-- It returns a row which does not contain any index below the given
-- integer, and otherwise, for every index in the given rows, this index if
-- it's present in only one of these rows.
sub :: Row -> Row -> Int -> Row
sub a b k = IS.filter (\v -> v >= k) rest
  where
    rest = (a `IS.union` b) `IS.difference` (a `IS.intersection` b)

data {- (RhsC rhs) => -} System rhs = S {
  sA :: Vector Row,      -- matrix A
  sB :: Vector rhs,      -- vector B (right hand side)
  sC :: Int,        -- number of columns (unknowns)
  sR :: Int,        -- number of rows
  sP :: UV.Vector Int,      -- permutation, tracks swapping of rows
  sN :: Int,        -- index of next column that needs to be used
  sS :: Maybe [rhs] -- solution to the problem, if any
  } deriving(Show)
         
makeSystem :: Int -> System rhs
makeSystem nC = S { 
  sA = V.empty,
  sB = V.empty,
  sC = nC,
  sR = 0,
  sP = UV.empty,
  sN = 0,
  sS = Nothing
  }
                
         
findBestPivot :: System rhs -> Int -> Maybe (Int, Int)
findBestPivot s i =  
  loop Nothing i
  where
    a = sA s
    loop best j =
      if j == sR s
      then best
      else loop best' (j+1)
        where
          aj = (V.!) a j
          best' =
            if IS.member i aj
            then
              let d = degree aj in
              case best of
                Just (_, bd) | bd <= d -> best
                _                      -> Just (j,d)
            else
              best

swap :: GV.Vector v a => v a -> Int -> Int -> v a
swap v i j = GV.modify act v
  where
    act mv = do
        vi <- GV.indexM v i
        vj <- GV.indexM v j
        MGV.write mv i vj
        MGV.write mv j vi

        
swapRows :: System rhs -> Int -> Int -> System rhs
swapRows s i j =
  s {
    sA = swap (sA s) i j,
    sB = swap (sB s) i j,
    sP = swap (sP s) i j 
    }
    

setRow :: System rhs -> Int -> Row -> rhs -> System rhs
setRow s i ri bi =
  s {
    sA = set (sA s) i ri,
    sB = set (sB s) i bi
    }
         


makeZeroes :: RhsC rhs => System rhs -> Int -> System rhs -- zero columns under i using i
makeZeroes s i =
  loop s (i+1)
  where 
    ai = get (sA s) i
    bi = get (sB s) i
    r = sR s
    loop s' j =
      if j == r 
      then s' { sN = i + 1}
      else
        loop s'' (j+1)
        where
          aj' = sub aj ai i
          bj  = get (sB s') j
          bj' = bj -: bi 
          aj = get (sA s') j
          s'' = if IS.member i aj
               then setRow s' j aj' bj'
               else s'

data SS rhs = 
  Ok (System rhs)
  | Stuck (System rhs) Int
    deriving(Show)

cols :: SS rhs -> Int
cols (Ok s) = sC s
cols (Stuck s _) = sC s

reduceBelow :: Rhs.RhsC rhs => System rhs -> Int -> SS rhs
reduceBelow s i = 
  case findBestPivot s i of
    Nothing -> Stuck s i
    Just(p, _) -> Ok (makeZeroes s' i)
      where s' = if p ==i then s else swapRows s i p
          

toR :: RhsC rhs => SS rhs -> SS rhs
toR ss = -- transform s into an equivalent right triangle matrix
  loop ss 0 
    where
      loop ss' i | i == cols ss' = ss'
      loop (Ok s) i = loop (reduceBelow s i) (i+1)
      loop ss' _ = ss'

backSubstitute :: RhsC rhs => System rhs -> [rhs]
backSubstitute s =
  loop [] (sC s - 1)
  where
    loop xs j | j < 0 = xs
    loop xs j         = loop xs' (j-1)
      where 
        aj = get (sA s) j
        bj = get (sB s) j
        -- TODO Rework without intermediate lists
        aj' = [IS.member i aj | i <- [0 ..]]
        xj = foldl (\ acc (xk,ak) -> if ak then acc -: xk else acc) bj (zip xs (drop (j+1) aj'))
        xs' = xj : xs
      
unpermute :: Ord b1 => [b1] -> [b] -> [b]
unpermute ps xs =
  map fst sorted
  where 
    c (_, p0) (_, p1) = compare p0 p1
    sorted = sortBy c (zip xs ps)

solve :: RhsC rhs => System rhs -> System rhs
solve s = 
  case toR (Ok s) of
    Ok s'     -> s' { sS = Just (unpermute (UV.toList $ sP s) (backSubstitute s')) }
    Stuck s' _ -> s'
      
  
addRow :: System rhs -> Row -> rhs -> System rhs
addRow s r b = s { 
  sA = V.snoc (sA s) r,
  sB = V.snoc (sB s) b,
  sR = sR s + 1,
  sP = UV.snoc (sP s) (sR s),
  sN = 0}
