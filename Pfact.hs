import Util
import Data.Maybe

pfactor :: [Int] -> [[Int]]

pfactor p = let recPfactor pp m
			| m < 0 = [pp]
			| otherwise = let s = [0..m]
					  s' = map (eval p) s
					  f = map factorSet s'
					  np = ndSubsets f
					  pts = map (zip s) np
					  q = maybeTake (((=<<) (\y -> if (y == [1]) || (y == [-1]) then Nothing else Just y)) . (\x -> pdiv pp $ intInterpolate x)) pts
				      in case q of Nothing -> recPfactor pp (m-1)
						   Just qq -> (pfactor qq) ++ (pfactor $ fromJust $ pdiv pp qq)
	    in recPfactor p $ (length p `quot` 2) + 1


remove :: Eq b => [b] -> [[b]] -> [[b]]

remove x [] = []
remove x (y:ys) = if x == y
		  then remove x ys
		  else y:(remove x ys)


ndSubsets :: [[a]] -> [[a]]

ndSubsets [] = [[]]
ndSubsets (a:as) = case a of [x] -> map ([x] ++) $ ndSubsets as
                             othewise -> (map ([head a] ++) $ ndSubsets as) ++ (ndSubsets $ (tail a):as)





maybeTake :: (a -> Maybe b) -> [a] -> Maybe b

maybeTake f [] = Nothing
maybeTake f (x:xs) = case f x of Nothing -> maybeTake f xs
				 Just y -> Just y

intInterpolate :: [(Int, Int)] -> [Int]

intInterpolate l = map ceiling $ interpolate $ map (\(x,y) -> (toRational x, toRational y)) l



main :: IO()
main = print $ pfactor [-1,-3,-3,-1]
