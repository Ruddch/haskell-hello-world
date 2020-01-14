import Data.List
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
