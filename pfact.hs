import util.hs

pfactor :: (Num a , Ord a) => [a] -> [[a]]

pfactor p = let recPfactor pp m =
		if m < 0
		then [pp]
		else let s = [-m..m]
			 s' = map (eval p) s
			 f = map factorSet s'
			 np = ndSubsets f -- <---NP-Complete :(
			 q = maybeTake (\x y -> pdiv x y) np
		     in case q of Nothing -> recPfactor pp (m-1)
				  Just qq -> qq:(pfactor $ fromJust $ pdiv pp qq)
	    in recPfactor p floor $ length p / 2


ndSubsets :: [[a]] -> [[a]]

ndSubsets [] = [] ---in progress
ndSubsets [a]:(x:xs) = map (a:) x
ndSubsets (a:as):x = ndSubsets [a]:x ++ ndSubsets as:x

