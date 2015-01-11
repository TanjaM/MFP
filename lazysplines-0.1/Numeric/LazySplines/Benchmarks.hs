import Criterion.Main

import Numeric.LazySplines.Examples
import Numeric.DSolve

import GHC.IO.Encoding

-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

runTest f = f
	
-- | The @main@ function generates space and time measurements.
main =
	do
	setLocaleEncoding utf8
	setFileSystemEncoding utf8
	setForeignEncoding utf8
	defaultMain [
	  bgroup "first order differential equation" [ bench "dsolve for duck" $ whnf runTest (dsolve (\x -> duckDeathAtAge * (1 - x)) initialLife)
				   ]
	  ]