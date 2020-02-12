t 'a'
t (True, "Hello")

--Function type declaration
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

--Int Interger Float Double Bool Char 

--Type variables
:t head -- head :: [a] -> a

-- Typeclasses 101
:t (==)
--Eq Ord Show  Read
read "5" :: Int 
-- Enum Bounded Num 
-- Integral Floating