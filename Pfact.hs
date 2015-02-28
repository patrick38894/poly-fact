import Util
import Data.Maybe

pfactor :: [Int] -> [[Int]]

pfactor p = let recPfactor pp m
			| m < 0 = [pp]
			| otherwise = let s = [0..m]
					  s' = map (eval p) s
					  f = map factorSet s'
					  np = ndSubsets f -- <---NP-Complete :(
					  pts = map (zip s) np
					  q = maybeTake (\x -> pdiv pp $ map ceiling $ intInterpolate x) pts
				      in case q of Nothing -> recPfactor pp (m-1)
						   Just qq -> if qq == [1]
							      then [pp]
							      else (pfactor qq) ++ (pfactor $ fromJust $ pdiv pp qq)
	    in recPfactor p $ (length p `quot` 2) +1


ndSubsets :: [[a]] -> [[a]]

ndSubsets [] = [[]]
ndSubsets (a:as) = case a of [x] -> map ([x] ++) $ ndSubsets as
                             othewise -> (map ([head a] ++) $ ndSubsets as) ++ (ndSubsets $ (tail a):as)





maybeTake :: (a -> Maybe b) -> [a] -> Maybe b

maybeTake f [] = Nothing
maybeTake f (x:xs) = case f x of Nothing -> maybeTake f xs
				 Just y -> Just y

intInterpolate :: [(Int, Int)] -> [Rational]

intInterpolate l = interpolate $ map (\(x,y) -> (toRational x, toRational y)) l



main :: IO()
main = print $ pfactor [-1,-3,-3,-1]
