import Data.Matrix
import Data.Maybe


interpolate :: (Fractional a, Ord a) => [(a,a)] -> Matrix a

interpolate xs = let n = length xs
		     m = matrix n n $ \(i,j) -> (fst (xs !! (i-1))) ^ (n-j)
		     p = gaussJordElim m $ matrix n 1 $ \(i,j) -> snd (xs !! (i-1))
		 in matToList p


solveLower :: (Fractional a, Ord a) => Matrix a -> Matrix a -> [a]

solveLower l b
	| nrows b == 0 = []
	| otherwise =  (y / x):solveLower m' b'
	where x = l ! (1,1)
	      y = b ! (1,1)
	      m' = minorMatrix 1 1 l
	      b' = matrix (nrows b -1) 1 $ \(i,j) -> (b ! (i+1,j))*(1-y/x)

solveUpper :: (Fractional a, Ord a) => Matrix a -> Matrix a -> [a]


solveUpper u y 
	| nrows y == 0 = []
	| otherwise = (b / a):solveUpper u' y'
	where a = u ! (nrows u,ncols u)
	      b = y ! (1,nrows y)
	      u' = minorMatrix (nrows u) (ncols u) u
	      y' = matrix (nrows y -1) 1 $ \(i,j) -> (y ! (i+1,j))*(1-b/a)


gaussJordElim :: (Fractional a, Ord a) => Matrix a -> Matrix a -> Matrix a

gaussJordElim a b = let (l,u,p,d) = fromJust (luDecomp a)
			y = solveLower l (p * b)
			x = solveUpper u $ fromList (length y) 1 y
		    in fromList (length x) 1 x

matToList :: Matrix a -> [a]

matToList x = if nrows x == 0
	   then []
	   else (x ! (1,1)):(matToList $ minorMatrix 1 0 x)

