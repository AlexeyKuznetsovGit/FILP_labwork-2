f :: ([Double] -> Double -> Double -> [Double]) -> ([Double],[Double]) -> Double -> Double -> [Double]
f f2 (xs, ys) k m = f2 xs k m ++ f2 ys k m

f2 :: [Double] -> Double -> Double -> [Double]
f2 [] _ _ = []
f2 (x : xs) k m = if x >= 0 then ((x*(-1))/k) : f2 (xs) k m
         else ((x*(-1))*m) : f2 (xs) k m