{-# LANGUAGE DeriveDataTypeable #-} 
{- |
   Module      : Model.PreludeGapFiller
   Description : PreludeGapFiller Utility functions
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines PreludeGapFiller utility functions.  
   Mostly these are functions that don't seem to be available, or I just can't find them. 
   As I learn more Haskell, this module should get smaller. Or not. :)  
   
   These apply to any type, or to standard types like String, Int, or Bool, or standard Classes like Num, Show, Ord or Eq. 
   These only rely on standard modules.  
   Complexity or Big O values are best guess-estimates.  
   Some of these functions probably need rewriting, but lack of time, etc. 
   
 -}
module DBCommon.PreludeGapFiller
    ( 
    
      tupleEq,    
      tailSafe, 
      maxOr0, 
      mapBoth, 
      mapSnd, 
      filterSnd, 
      getLeft, 
      getRight, 
      getDefault, 
      trim, 
      isOracleUpperCaseName, 
      xNotInY2,
      listFrequency, 
      getDups, 
      tuple2Map, 
      reorderTuples, 
      epochDay,
      epochUTCTime, 
      getStaticType, 
      
     ) where 
 
--------------------------------------------------------------------------------

import Data.Char 
import Data.List 
import qualified Data.Map.Strict as M  
import Data.Ord 
import Data.Time 
import Data.Typeable 

--------------------------------------------------------------------------------
--    Prelude 
--    a tuple comparison 
--    Complexity O ( 1 )  
tupleEq :: Eq a => ( a,a ) -> Bool 
tupleEq ( a,b ) = a == b   

--     a safe or total (non-partial) version of tail 
-- > tailSafe [] = []
-- > tailSafe [1,3,4] = [3,4]
--    Complexity O ( 2 ) 
tailSafe :: [a] -> [a]
tailSafe = liftSafe tail null 

--     a safe or total (non-partial) version of max 
--    Complexity O ( n )  
maxOr0 :: (Ord a, Num a) => [a] -> a 
maxOr0 [] = 0 
maxOr0 xs = maximum xs 

--    use two elements to make a tuple 
--    then repopulate with fst and snd of tuple 
--    must be same type  
--    Complexity O ( n )     
mapBoth :: ( a -> a -> ( a, a ) ) -> [a] -> [a] 
mapBoth _ [] = [] 
mapBoth _ [x] = [x] 
--    modifies the x and the y, and continues down the list 
mapBoth f ( x:y:zs ) = fst ( f x y ) : mapBoth f ( snd ( f x y ):zs )  
    
--    use two elements to modify the second element 
--    must be same type 
--    look at scanr1? 
--    Complexity O ( n )    
mapSnd :: ( a -> a -> a ) -> [a] -> [a] 
mapSnd _ [] = [] 
mapSnd _ [x] = [x] 
--    retains the x, modifies the y, and continues down the list 
mapSnd f ( x:y:zs ) = x : mapSnd f ( ( f x y ):zs )   

--    use two elements to filter the second element 
--    if true, keeps y, and continue 
--    if false, discard y and continue 
--    Complexity O ( n )    
filterSnd :: ( ( a, a ) -> Bool ) -> [a] -> [a] 
filterSnd _ [] = [] 
filterSnd _ [x] = [x] 
filterSnd prdct ( x:y:zs ) 
    | prdct ( x,y ) = x : filterSnd prdct ( y:zs ) 
    | otherwise = x : filterSnd prdct zs  
    
--    const :: a -> b -> a
--    const x _ =  x 
--    isLeft :: Either a b -> Bool 
--    isRight :: Either a b -> Bool  
--    either :: ( a -> c ) -> ( b -> c ) -> Either a b -> c 
--    either :: ( a -> a ) -> ( b -> a ) -> Either a b -> a 
--    either :: ( a -> b ) -> ( b -> b ) -> Either a b -> b  
--    gets the Left or error side of Either 
--    Note that a must be a Monoid so that  
--    Complexity O ( 1 )    
getLeft :: Monoid a => Either a b -> a 
getLeft eLR = either id ( const mempty ) eLR 
  
--    gets the Right or correct side of Either
--    Note that b must be a Monoid 
--    Complexity O ( 1 )    
getRight :: Monoid b => Either a b -> b 
getRight eLR = either ( const mempty ) id eLR 

--    takes the default value 
getDefault :: ( String -> b ) -> Either String b -> b 
getDefault defaultFn eLR = either defaultFn id eLR 

--    Char 
--    removes spaces from start and end of a string 
--    Complexity O ( 2n ) 
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace 
    
-- Tests that a name is composed of valid Oracle characters 
-- Use for upper case physical names only 
--    Complexity O ( n )  
isOracleUpperCaseName :: String -> Bool
isOracleUpperCaseName = all isOracleUpperChar 

--    List 
--    general function to find XOR Set op 
--    2 functions are needed to extract the keys
--    the keys are compared, and the missing a values are returned 
--    nub is O( n^2 ) 
--    Complexity O( 2*n^2 + m^2 + 3n + m ) where n = length xs; m = length ys 
xNotInY2 :: ( Eq a, Eq k ) => ( a -> k ) -> ( b -> k ) -> [a] -> [b] -> [a] 
xNotInY2 x2k y2k xs ys 
    = nub ( filter ( ( ( flip elem ) notInP ).x2k ) xs )  
    where 
        xs1 = nub ( map x2k xs ) 
        ys1 = nub ( map y2k ys ) 
        notInP = xs1 \\ ys1 

--    determines the frequency of each element 
--    sort is O( n*(log n) ) 
--    Complexity O( n*(log n) + 2n ) 
listFrequency :: Ord a => [a] -> [( a, Int )] 
listFrequency ls = map ( \l -> ( head l, length l ) ) ( group ( sort ls ) ) 

--    gets all duplicate elements 
--    Complexity O( n^2 + 3n ) 
--    TEST ( getDups "abbcd" ) -> 
getDups :: ( Eq a ) => [a] -> [a] 
getDups xs = nub ( concat ( filter ( \x -> ( ( length x ) > 1 ) ) ( ( groupBy ( == ) xs ) ) ) ) 

--    Map, Ord   
--    convert to a Map k v 
--    M.fromAscList has O( n ). Must Sort first! 
--    Complexity O( n*(log n) + n ) 
tuple2Map :: ( Eq k, Ord k ) => [( k, a )] -> M.Map k a 
tuple2Map ts   
    = M.fromAscList ( sortBy ( comparing fst ) ts )  
    
--    inverts and re-orders 
--    note that default values are reversed as well. 
--    Complexity O( n*log n + n * m ) where n = length ts and m = average bs 
reorderTuples :: ( Ord a, Ord b ) => a -> b -> [( a, [b] )] -> [( b, [a] )]  
reorderTuples aDef bDef ts 
    = sortFoldlTupleAny bDef aDef invertTs  
        where 
            invertTs = concatMap invertTuple ts  

--    Time  
--    "1-Jan-70" 
--    showGregorian epochDay = 1970-01-01 
--    The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
--    Complexity O ( 1 )    
epochDay :: Day 
epochDay = fromGregorian 1970 1 1 

--    Day and time from midnight (up to 86401 seconds)
--    Complexity O ( 1 ) 
epochUTCTime :: UTCTime 
epochUTCTime = parseTimeOrError True defaultTimeLocale "%-d-%b-%y" "1-Jan-70" 

--    Typeable 
--    gets static type 
--    Complexity O ( 1 ) 
getStaticType :: ( Typeable a ) => a -> String 
getStaticType = show . typeOf  
    
--------------------------------------------------------------------------------
--    hidden 
--    Prelude 

--     a safe or total (non-partial) version of init  
--    Complexity O ( n )  
initSafe :: [a] -> [a]
initSafe = liftSafe init null

--    adds safety to unsafe or partial functions 
--    Complexity O ( 2 ) 
liftSafe :: ( a -> a ) -> ( a -> Bool ) -> ( a -> a )
liftSafe func test val = if test val then val else func val

--    inverts a tuple 
--    Complexity O( n ) where n = length bs 
invertTuple :: ( a, [b] ) -> [( b, a )]  
invertTuple ( a, bs ) = map ( \b -> ( b, a ) ) bs 

--    callable by a fold to accumulate lists with the same key value 
--    works best if the ( k,_ ) is sorted 
--    Complexity O( n ) where n = length acc  
foldlTuple :: ( Eq k ) => [( k,[v] )] -> ( k,v ) -> [( k,[v] )] 
foldlTuple acc ( k,v ) 
--    | trkce ( "tF " ++ show s ++ " " ++ show x ++ " "  ) False = undefined 
--    make last safe, and create first acc value 
    | length acc == 0 = [( k,[v] )] 
--    if previous key is the same, append to last acc value  
    | fst la == k = ( ( initSafe acc ) ++ [( fst la,( snd la ++ [v] ) )] ) 
--    else start new key 
    | otherwise = ( acc ++ [( k,[v] )] ) 
    where la = last acc 

--    Char 
-- Valid Oracle names can only contain uppers, Numerics and the chars _ $ # and @ 
--    Complexity O ( 3 ) 
isOracleUpperChar :: Char -> Bool 
isOracleUpperChar c = isUpper c || isDigit c || isOracleSpecialChar c 

-- Valid Oracle special characters; avoid @ 
--    Complexity O ( 3 ) 
isOracleSpecialChar :: Char -> Bool
isOracleSpecialChar c = c == '_' || c == '$' || c == '#' 

--    List 
--	creates a list of (key, value list) tuples 
--    foldl wrapper 
--    e.g. FROM filter ( /= ( "",[0] ) ) ( foldl foldlTuple [( "",[0] )] tBL1 ) 
--    need to filter out defaults kDef vDef 
--    nub removes duplicates 
--    Complexity O( n^2 + 3n ) where n = length ts   
foldlTupleWrap :: ( Eq k, Eq v, Ord v ) => k -> v -> [( k,v )] -> [( k,[v] )] 
foldlTupleWrap kDef vDef ts 
    = map ( \( k,vs ) -> ( k, nub vs ) ) ( filter ( /= ( kDef,[vDef] ) ) ( foldl' foldlTuple [( kDef,[vDef] )] ts ) ) 

--    Ord 
--    sort, fold and tuple up pattern for any type  ( filter ( ( /= [] ).snd ) 
--    Complexity O( n*log n ) where n = length ts   
sortFoldlTupleAny :: ( Ord k, Ord v ) => k -> v -> [( k,v )] -> [( k,[v] )] 
sortFoldlTupleAny kDef vDef ts 
    = foldlTupleWrap kDef vDef ( sortBy ( comparing fst ) ts )  

