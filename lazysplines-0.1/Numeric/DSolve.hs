{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Numeric.DSolve where
import Numeric.LazySplines
import Data.List
import Control.Arrow

-- TODO
dsolve f x0 = x
	where
		x' = f x
		x = x0 ++ integrateSpline x'

dsolveWithEps f x0 eps = dsolve f (liftS2 eps x0) 

dsolveWithTrim f x0 trim = x
	where 
		x' = f x
			`trimmingTo` trim
		x = x0 ++ integrateSpline x'
		
dsolveWithEpsAndTrim f x0 eps trim = dsolveWithTrim f (liftS2 eps x0)  trim

dsolveWithHigherOrder f x0 n = dsolve f dx0
	where
		dx = take (n + 1) (dsolve f x0)
		dx0 = [(last dx)] - (sumSpline (init dx))

dsolve2 f x0 x0' = x
	where
		x'' = f x x'
		x' = x0' ++ integrateSpline x''
		x = x0 ++ integrateSpline x'
		
dsolve2WithEps f x0 eps x0' eps' = dsolve2 f (liftS2 eps x0) (liftS2 eps' x0') 

dsolve2WithTrim f x0 trim x0' trim' = x
	where
		x'' = f x x'
		x' = x0' ++ integrateSpline x''
			`trimmingTo` trim'
		x = x0 ++ integrateSpline x'
			`trimmingTo` trim

dsolve2WithEpsAndTrim f x0 eps trim x0' eps' trim' = dsolve2WithTrim f (liftS2 eps x0) trim (liftS2 eps' x0') trim'

