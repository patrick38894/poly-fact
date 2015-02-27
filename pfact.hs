import util.hs

pfactor :: (Num a , Ord a) => [a] -> [[a]]

pfactor p = let m = floor $ length p / 2
		s = [-m..m]
		s' = map (eval p) s
		f = map factorSet s'
		
		
