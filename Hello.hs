module Pam where
 
import Data.Function
import Data.Char
import Data.List

sumSquares x y = x ^ 2 + y ^ 2
main = putStrLn "Hello World!"

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n == (-1) = 1
            | n > 1 = fibonacci(n - 1) + fibonacci(n - 2)
            | n < (-1) = fibonacci(n + 2) - fibonacci(n + 1)


fibonacci' :: Integer -> Integer
fibonacci' n | n >= 0 = iter n 1 0
             | n < 0 = iter n 1 0

iter :: Integer -> Integer -> Integer -> Integer
iter n a b | n == 0 = b 
             | n > 0 = iter (n - 1) (a + b) a
             | n < 0 = iter (n + 1) (b - a) a


seqA :: Integer -> Integer

seqA n = let
    iter a b c n | n == 2 = c
    		 | n == 1 = b
     		 | n == 0 = a
                 | otherwise = iter b c (c + b - 2 * a) (n - 1)
    in iter 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = let 
    iter x sumAcc countAcc | x < 10 = (sumAcc + x, countAcc)
      | otherwise = iter (quot x 10) (sumAcc + (rem x 10)) (countAcc + 1)
    in iter (abs x) 0 1

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
    step = (b - a) / steps 
    steps = 1000
    iter f a b acc n| n == 0 = acc * step / 2 
                    | otherwise = iter f b (b + step) ((f a + f b)  + acc) (n - 1)
    in iter f a (a + step) 0 steps

getSecondFrom :: a -> b -> c -> b
getSecondFrom first second third = second

multSecond = g `on` h

g = (*)

h = snd


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


doItYourself = f1 . g1 . h1

f1 = logBase 2

g1 = flip (^) 3

h1 = max 42

class Printable a where
    toString :: a -> String
    
instance Printable Bool where
    toString True = "True"
    toString False = "False"

instance Printable () where
	toString a = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where
	toString (a,b) =  "(" ++ (toString a) ++ "," ++ (toString b) ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool


class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | (and) [doesEnrageGork a,  doesEnrageMork a] = stab $ stomp a 
                | doesEnrageGork a = stomp a
                | doesEnrageMork a = stab a
                | otherwise = a

class (Eq a, Bounded a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x == maxBound = minBound
          | otherwise = succ x

  spred :: a -> a
  spred x | x == minBound = maxBound
          | otherwise = pred x

avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger (toInteger a + toInteger b + toInteger c) / 3 

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y list = x : y : list

nTimes:: a -> Int -> [a]
nTimes num count = let 
    iter res n | n == count = res
               | otherwise = iter (num : res) (n + 1)
    in iter [] 0               

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | otherwise = oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = reverse a == a 

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 x y z = let
    sum2 xs [] = xs
    sum2 [] ys = ys
    sum2 (x:xs) (y:ys) = (x + y) : sum2 xs ys
    in sum2 x $ sum2 y z


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (a:as) = let
    helper (x:xs) (y:acc)
        | x == y = helper xs (x:y:acc)
        | otherwise = (y : acc ) : helper xs [x]
    helper [] acc = [acc]  
    in helper as [a]

readDigits :: String -> (String, String)
readDigits = span isDigit

qsort :: Ord a => [a] -> [a]

qsort [] = []
qsort [x] = [x]
qsort (x:xs) = (qsort f) ++ [x] ++ (qsort s) 
    where f = filter (< x) xs
	  s = filter (>= x) xs

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
    
perms (x:xs) = concatMap (\list -> interleave list) (perms xs)
        where interleave [] = [[x]]
              interleave (y:ys) = (x:y:ys) : map (y:) (interleave ys)


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
m3 :: Ord a => a -> a -> a -> a
m3 a b c = maximum [a,b,c]

max3 = zipWith3 m3

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- change :: (Ord a, Num a) => a -> [[a]]
-- change 0 = [[]]
-- change sum = [ c:cs | c<-coins, c<=sum, cs<-change (sum - c) ]

evenOnly :: [a] -> [a]
evenOnly arr = fst $ foldl (\(list, index) x -> if even index then (list ++ [x], index + 1) else (list, index + 1)) ([], 1) arr

evenOnly' :: [a] -> [a]
evenOnly' arr = fst $ foldl (\(list, index) x -> if even index then (list ++ [x], index + 1) else (list, index + 1)) ([], 1) arr

revRange :: (Char,Char) -> [Char]
revRange (start, end) = unfoldr g end
  where g x = if x < start then Nothing else Just(x, pred x)

processData :: SomeData -> String
processData d = case doSomeWork d of 
    (_, 0) -> "Success"
    (_, x) -> "Fail: " ++ show x

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x y) (Point x1 y1) = sqrt((x - x1) ^ 2 + (y - y1) ^ 2 )

data Result' = Err Int | Succ

instance Show Result' where
    show (Err code) = "Fail: " ++ show code
    show Succ = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' d = let (r, code) = doSomeWork d in
    if code == 0 then Succ else Err code
    


data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName person@(Person fName _ _) = person {firstName = newFName}
    where newFName = if length fName > 2 
                     then take 1 fName ++ "." 
                     else fName

data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x y) (Coord x1 y1) = sqrt((x1 - x)^2 + (y1 - y)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord x1 y1) = abs(x1 - x) + abs(y1 - y)

data Bit = Zero | One | Pam deriving Show
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving Show

zToInt :: Z -> Int

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

intToBit :: Int -> Bit
intToBit 1 = One
intToBit 0 = Zero
intToBit _ = Pam

zToInt (Z sign bits) = if sign == Plus
                          then (fst number)
                          else -1 * (fst number)
    where number = foldl f (0,0) bits
          f = (\(sum, count) bit -> (sum + (bitToInt bit)*2^count, count + 1))

intToZ :: Int -> Z
intToZ number = if number < 0
                   then (Z Minus bits)
                   else (Z Plus bits)
    where bits = unfoldr (\x -> if x /= 0 then Just(intToBit(rem (abs x) 2), quot x 2) else Nothing) number

add :: Z -> Z -> Z
add a b = intToZ (zToInt a + zToInt b)

mul :: Z -> Z -> Z
mul a b = intToZ (zToInt a * zToInt b)

