import Data.Matrix


interpolate :: [(Num, Num)] -> Matrix Num

interpolate xs = let n = length xs
		     m = matrix n n $ \(i,j) -> (fst (xs !! (i-1))) ^ (n-j)
		     p = gaussJordElim m $ matrix n 1 $ \(i,j) -> snd (xs !! (i-1))
		 in toList p


solveLower :: Matrix Num -> Matrix Num -> [Num]

solveLower l b
	| nrows b == 0 = []
	| otherwise =  (y / x):solveLower m` b`
	where x = l ! (1,1)
	      y = b ! (1,1)
	      m` = minorMatrix 1 1 m
	      b` = matrix (nrows b -1) 1 $ \(i,j) -> (b ! (i+1,j))*(1-y/x)

solveUpper :: Matrix Num -> Matrix Num -> [Num]


solveUpper u y 
	| nrows y == 0 = []
	| otherwise = (b / a):f u' y'
	where a = u ! (nrows u,ncols u)
	      b = y ! (1,nrows b)
	      u' = minorMatrix (nrows u) (ncols u) u
	      y' = matrix (nrows y -1) 1 $ \(i,j) -> (y ! (i+1,j))*(1-b/a)


gaussJordElim :: Matrix Num -> Matrix Num -> Matrix Num

gaussJordElim a b = let (l,u,p,d) = luDecomp a
			y = solveLower l (p * b)
			x = solveUpper u $ fromList (length y) 1 y
		    in fromList (length x) 1 x

toList :: Matrix a -> [a]

toList x = if nrows x == 0
	   then []
	   else (x ! (1,1)):minorMatrix 1 0 x

