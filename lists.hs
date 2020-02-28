--lists
-- ++ : !! head tail last init 
-- length null reverse
-- take
take 3 [5,4,3,2,1] --[5,4,3] 
-- drop
drop 3 [8,4,2,1,5,6] -- [1,5,6]
-- minimum maximum
--sum product product
-- `elem`
4 `elem` [3,4,5,6] -- True

-- 
[1..20] --[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
take 24 [13,26..]
-- cycle repeat replicate
take 10 (cycle [1,2,3]) 
take 10 (repeat 5)
replicate 3 10

--  list comprehension
[x*2 | x <- [1..10]]
[ x | x <- [50..100], x `mod` 7 == 3]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[ x*y | x <- [2,5,10], y <- [8,10,11]]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   
removeNonUppercase "Hahaha! Ahahaha!"  "HA"

--Tuples
-- fst snd
fst (8,11)
-- zip
zip [1,2,3,4,5] [5,5,5,5,5]
zip [1..] ["apple", "orange", "cherry", "mango"]
-- [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

-- right triangles
let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

--Lambdas
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

--flip' :: (a -> b -> c) -> b -> a -> c  
--flip' f = \x y -> f y x  

--fold

--foldl
--sum' :: (Num a) => [a] -> a  
--sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

--foldr 
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs 

--foldl1 foldr1
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x) 

--scanl scanr scanl1 scanr1
scanl (+) 0 [3,5,2,1]
scanr (+) 0 [3,5,2,1]
scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
scanl (flip (:)) [] [3,2,1]

sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1  

--Function application with $
($) :: (a -> b) -> a -> b  
f $ x = f x

map ($ 3) [(4+), (10*), (^2), sqrt]


--Function composition
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  

--map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

--point free style

--fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50


--oddSquareSum :: Integer  
--oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

--oddSquareSum :: Integer  
--oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  

oddSquareSum :: Integer  
oddSquareSum =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit