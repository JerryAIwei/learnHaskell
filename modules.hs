import Data.List  
--import Data.List (nub, sort)  
--import Data.List hiding (nub)
--import qualified Data.Map
--import qualified Data.Map as M     
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub 

--Data.List
intersperse '.' "MONKEY" 
intercalate intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
transpose [[1,2,3],[4,5,6],[7,8,9]]
map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
concat [[3,4,5],[2,3,4],[2,1,1]]
concatMap (replicate 4) [1..3]
and $ map (>4) [5,6,7,8] 
or $ map (==4) [2,3,4,5,6,1] 
any (==4) [2,3,5,6,1,4]
all (>4) [6,9,10]
take 10 $ iterate (*2) 1
splitAt 3 "heyman"
takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
dropWhile (/=' ') "This is a sentence"
break (==4) [1,2,3,4,5,6,7]
sort [8,5,3,2,1,6,4,2]  
group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
inits "w00t"  
tails "w00t"
"cat" `isInfixOf` "im a cat burglar"
"hey" `isPrefixOf` "oh hey there!"
"there!" `isSuffixOf` "oh hey there!" 

partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"

find (>4) [1,2,3,4,5,6]
4 `elemIndex` [1,2,3,4,5,6]
' ' `elemIndices` "Where are the spaces?"
findIndex (==4) [5,3,2,1,6,4] 
zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
lines "first line\nsecond line\nthird line"
unlines ["first line", "second line", "third line"]

words "hey these are the words in this sentence"
nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
delete 'h' "hey there ghang!"
[1..10] \\ [2,5,9] 
"hey man" `union` "man what's up"
[1..7] `intersect` [5..10]
insert 4 [3,5,1,2,8,2]
groupBy (\x y -> (x > 0) == (y > 0)) values

--Data.Map
--Data.Char
--Data.Set

module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  
  
sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  
  
cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  














