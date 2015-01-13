import Criterion.Main
import Numeric.LazySplines.Examples
import Numeric.DSolve
import GHC.IO.Encoding

runTest f = f
	
main =
	do
	setLocaleEncoding utf8
	setFileSystemEncoding utf8
	setForeignEncoding utf8
	defaultMain [
	  -- f1:  y'(x)=y(x)
      --      y(0)=1
	  bgroup	"f1_basic" 		[ bench "f1"         $ whnf runTest (dsolve (\x -> x) 1)],
	  bgroup	"f1_eps" 		[ bench "f1_eps_001" $ whnf runTest (dsolveWithEps (\x -> x) 1 0.01)
								, bench "f1_eps_01"  $ whnf runTest (dsolveWithEps (\x -> x) 1 0.1)
								, bench "f1_eps_05"  $ whnf runTest (dsolveWithEps (\x -> x) 1 0.5)
								, bench "f1_eps_1"   $ whnf runTest (dsolveWithEps (\x -> x) 1 1)
								, bench "f1_eps_10"  $ whnf runTest (dsolveWithEps (\x -> x) 1 10)
								],
	  bgroup 	"f1_trim"   	[ bench "f1_trim_2"   $ whnf runTest (dsolveWithEpsAndTrim (\x -> x) 1 0.5 2)
								, bench "f1_trim_3"   $ whnf runTest (dsolveWithEpsAndTrim (\x -> x) 1 0.5 3)
								, bench "f1_trim_5"   $ whnf runTest (dsolveWithEpsAndTrim (\x -> x) 1 0.5 5)
								, bench "f1_trim_10"  $ whnf runTest (dsolveWithEpsAndTrim (\x -> x) 1 0.5 10) 
								, bench "f1_trim_100" $ whnf runTest (dsolveWithEpsAndTrim (\x -> x) 1 0.5 100)
								] ,
	  bgroup 	"f1_order"   	[ bench "f1_order_1"  $ whnf runTest (dsolveWithHigherOrder (\x -> x) [(0.5,[1])] 1)
								, bench "f1_order_2"  $ whnf runTest (dsolveWithHigherOrder (\x -> x) [(0.5,[1])] 2)
								, bench "f1_order_3"  $ whnf runTest (dsolveWithHigherOrder (\x -> x) [(0.5,[1])] 3)
								, bench "f1_order_5"  $ whnf runTest (dsolveWithHigherOrder (\x -> x) [(0.5,[1])] 4) 
								, bench "f1_order_10" $ whnf runTest (dsolveWithHigherOrder (\x -> x) [(0.5,[1])] 10)
								],
	  --f2  y''(x)=-y(x)
      -- 	y(0)=0, y'(0)=1
	  bgroup	"f2_basic" 		[ bench "f2"          $ whnf runTest (dsolve2 (\x x' -> -x) 0 1)],
	  bgroup	"f2_eps" 		[ bench "f2_eps_0001" $ whnf runTest (dsolve2WithEps (\x x' -> -x) 0 0.001 1 0.001)
								, bench "f2_eps_001"  $ whnf runTest (dsolve2WithEps (\x x' -> -x) 0 0.01  1 0.01)
								, bench "f2_eps_01"   $ whnf runTest (dsolve2WithEps (\x x' -> -x) 0 0.1   1 0.1)
								, bench "f2_eps_05"   $ whnf runTest (dsolve2WithEps (\x x' -> -x) 0 0.5   1 0.5)
								],
	  bgroup 	"f2_trim"   	[ bench "f2_trim_2"   $ whnf runTest (dsolve2WithEpsAndTrim (\x x' -> -x) 0 0.1 2   1 0.1 2)
								, bench "f2_trim_3"   $ whnf runTest (dsolve2WithEpsAndTrim (\x x' -> -x) 0 0.1 3   1 0.5 3)
								, bench "f2_trim_5"   $ whnf runTest (dsolve2WithEpsAndTrim (\x x' -> -x) 0 0.1 5   1 0.5 5)
								, bench "f2_trim_10"  $ whnf runTest (dsolve2WithEpsAndTrim (\x x' -> -x) 0 0.1 10  1 0.5 10) 
								, bench "f2_trim_100" $ whnf runTest (dsolve2WithEpsAndTrim (\x x' -> -x) 0 0.1 100 1 0.5 100)
								]  
	  ]