norm :: Double -> Double
norm d = if d > 0
          then 1 - 1.001 ** (-d)
          else 0