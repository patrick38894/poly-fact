import Data.Matrix
import Data.Maybe


interpolate :: (Fractional a, Ord a) => [(a,a)] -> [a]

interpolate xs = let n = length xs
		     m = matrix n n $ \(i,j) -> (fst (xs !! (i-1))) ^ (n-j)
		     p = gaussJordElim m $ matrix n 1 $ \(i,j) -> snd (xs !! (i-1))
		 in matToList p


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
	      b' = matrix (nrows b -1) 1 $ \(i,j) -> (b ! (i+1,j))*(1-y/x)

solveUpper :: (Fractional a, Ord a) => Matrix a -> Matrix a -> [a]


solveUpper u y 
	| nrows y == 0 = []
	| nrows y == 1 = [b/a]
	| otherwise = (b / a):solveUpper u' y'
	where a = u ! (nrows u,ncols u)
	      b = y ! (nrows y,1)
	      u' = let size = nrows u
		   in case size of 1 -> submatrix 1 1 1 1 u
				   otherwise -> submatrix 1 (nrows u -1) 1 (ncols u -1) u
	      y' = matrix (nrows y -1) 1 $ \(i,j) -> (y ! (i+1,j))*(1-b/a)


gaussJordElim :: (Fractional a, Ord a) => Matrix a -> Matrix a -> Matrix a

gaussJordElim a b = let (l,u,p,d) = fromJust (luDecomp a)
			y = solveLower l (p * b)
			x = solveUpper u $ fromList (length y) 1 y
		    in fromList (length x) 1 x

matToList :: Matrix a -> [a]

matToList x
	| nrows x == 0 = []
	| nrows x == 1 = [x ! (1,1)]
	| otherwise = (x ! (1,1)):(matToList $ submatrix 2 (nrows x) 1 1 x)

