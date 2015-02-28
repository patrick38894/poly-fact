import Util
import Data.Maybe

pfactor :: [Int] -> [[Int]]

pfactor p = let recPfactor pp m
			| m < 0 = [pp]
			| otherwise = let s = [-m..m]
					  s' = map (eval p) s
					  f = map factorSet s'
					  np = ndSubsets f -- <---NP-Complete :(
					  q = maybeTake ((\x y -> pdiv x y) pp) np
				      in case q of Nothing -> recPfactor pp (m-1)
						   Just qq -> qq:(pfactor $ fromJust $ pdiv pp qq)
	    in recPfactor p $ length p `quot` 2


ndSubsets :: [[a]] -> [[a]]

ndSubsets [] = [[]]
ndSubsets (a:as) = case a of [x] -> map (++ [x]) $ ndSubsets as
                             othewise -> (map (++ [head a]) $ ndSubsets as) ++ (ndSubsets $ (tail a):as)





maybeTake :: (a -> Maybe a) -> [a] -> Maybe a

maybeTake f [] = Nothing
maybeTake f (x:xs) = case f x of Nothing -> maybeTake f xs
				 Just _ -> Just x


