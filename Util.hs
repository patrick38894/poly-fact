module Util
( factor
, signedFactor
, factorSet
, interpolate
, solveLower
, solveUpper
, gaussJordElim
, eval
, matToList
, pdiv
, padd
, pshiftl
, pmul
) where

import Data.Matrix
import Data.Maybe
import Data.List

--------------------prime factorization algorithms (naive) ------------------


factor :: Int -> [Int]

factor i = let f = (\n c -> if (c * c) > n
			    then [n]
			    else if (n `mod` c) == 0
				 then c:f (n `quot` c) c
				 else f n (c + 1))
	   in f i 2

	   
signedFactor :: Int -> [Int]

signedFactor i = let p = factor $ abs i
		 in p ++ map negate p


factorSet :: Int -> [Int]

factorSet i = reverse $ sort $ nub $ signedFactor i




---------------polynomial interpolation---------------------

interpolate :: (Fractional a, Ord a) => [(a,a)] -> [a]

interpolate xs = let n = length xs
		     m = matrix n n $ \(i,j) -> (fst (xs !! (i-1))) ^ (n-j)
		     p = gaussJordElim m $ matrix n 1 $ \(i,j) -> snd (xs !! (i-1))
		 in matToList p


-------------gauss-jordan-elimination for solving systems of linear equations-------------


solveLower :: (Fractional a, Ord a) => Matrix a -> Matrix a -> [a]

solveLower l b
	| nrows b == 0 = []
	| nrows b == 1 = [y/x]
	| otherwise =  (y / x):solveLower l' b'
	where x = l ! (1,1)
	      y = b ! (1,1)
	      l' = let size = nrows l
		   in case size of 1 -> submatrix 1 1 1 1 l
				   otherwise -> submatrix 2 (nrows l) 2 (ncols l) l
	      b' = matrix (nrows b -1) 1 $ \(i,j) -> (b ! (i+1,j)) -(y/x*(l ! (i+1,1)))

solveUpper :: (Fractional a, Ord a) => Matrix a -> Matrix a -> [a]


solveUpper u y -- note :: unlike solveLower, answer comes out reversed 
	| nrows y == 0 = []
	| nrows y == 1 = [b/a]
	| otherwise = (b / a):solveUpper u' y'
	where a = u ! (nrows u,ncols u)
	      b = y ! (nrows y,1)
	      u' = let size = nrows u
		   in case size of 1 -> submatrix 1 1 1 1 u
				   otherwise -> submatrix 1 (nrows u -1) 1 (ncols u -1) u
	      y' = matrix (nrows y -1) 1 $ \(i,j) -> (y ! (i,j)) -(b/a)*(u ! (i,ncols u))


gaussJordElim :: (Fractional a, Ord a) => Matrix a -> Matrix a -> Matrix a

gaussJordElim a b = let (u,l,p,d) = fromJust (luDecomp a)
			y = solveLower l (p * b) 
			x = reverse $ solveUpper u $ fromList (length y) 1 y
		    in fromList (length x) 1 x

matToList :: Matrix a -> [a]

matToList x
	| nrows x == 0 = []
	| nrows x == 1 = [x ! (1,1)]
	| otherwise = (x ! (1,1)):(matToList $ submatrix 2 (nrows x) 1 1 x)





-----------------------polynomial utilities-------------------

eval :: (Num a, Ord a) => [a] -> a -> a

eval [] x = 0
eval (p:ps) x = p * x ^ (length ps) + eval ps x

pdiv :: [Int] -> [Int] -> Maybe [Int]
pdiv x y = pdiv' (stripZeros x) (stripZeros y)

pdiv' :: [Int] -> [Int] -> Maybe [Int]

pdiv' [] _ = Just []
pdiv' pp@(p:ps) qq@(q:qs)
	| p `mod` q /= 0 = Nothing
	| length pp == length qq = if foldl (\x y -> x && y == 0) True $ zipWith (-) pp $ map (*c) qq
				   then Just [c]
				   else Nothing
	| otherwise = (mcons c) =<< pdiv' (zipWith' (-) ps $ map (*c) qs) qq
	where c = p `quot` q
	      mcons a b = Just (a:b)
	      zipWith' :: (a ->  a -> a) -> [a] -> [a] -> [a]
	      zipWith' f a [] = a
	      zipWith' f [] _ = []
	      zipWith' f aa@(a:as) bb@(b:bs) = if length aa == length bb
					       then zipWith f aa bb
					       else (f a b):(zipWith' f as bs)


stripZeros :: [Int] -> [Int]
stripZeros [] = []
stripZeros (0:xs) = stripZeros xs
stripZeros (x:xs) = (x:xs)

degree :: [Int] -> Int
degree [] = 0
degree [a] = 0
degree (x:xs) = if x == 0
		then degree xs
		else length xs

	      
	

pmul :: [Int] -> [Int] -> [Int]

pmul _ [] = [0]
pmul [] _ = [0]
pmul xx@(x:xs) yy@(y:ys) = padd (pshiftl (map (*x) yy) $ length xs) $ pmul xs yy


pshiftl :: [Int] -> Int -> [Int]

pshiftl p n = p ++ (take n $ repeat 0)


padd :: [Int] -> [Int] -> [Int]


padd [] _ = []
padd _ [] = []
padd xx@(x:xs) yy@(y:ys) = let xl = length xx
			       yl = length yy
			   in case compare xl yl of GT -> x:(padd xs yy)
						    LT -> y:(padd xx ys)
						    EQ -> zipWith (+) xx yy


