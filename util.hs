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

signedFactor i = let p = factor i
		 in p ++ map negate p


factorSet :: Int -> [Int]

factorSet i = sort $ nub $ signedFactor i




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


pdiv [] _ = Just []
pdiv pp@(p:ps) qq@(q:qs)
	| p `mod` q /= 0 = Nothing
	| length qq > length pp = if allZeros pp then Just [] else Nothing
	| otherwise = (pdiv pp' qq) >>= (\x -> Just (c:x))
	where c = (p `quot` q)
	      cd = length pp - length qq --degree of the constant factor
	      pp' = zipWith (-) ps $ (map (*c) qs) ++ take cd (repeat 0)
	      allZeros x = foldl (\y z -> y && (z == 0)) True x
	      
	











