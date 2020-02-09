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

