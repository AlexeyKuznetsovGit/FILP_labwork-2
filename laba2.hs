f2 :: ([Double] -> Double -> Double -> [Double]) -> [[Double]] -> Double -> Double -> [[Double]]
f2 f3 xs k m = [f3 x k m | x<-xs]

f3 :: [Double] -> Double -> Double -> [Double]
f3 [] _ _ = []
f3 (x : xs) k m = if x >= 0 then ((x*(-1))/k) : f3 (xs) k m
         else ((x*(-1))*m) : f3 (xs) k m