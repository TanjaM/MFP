{-|
Module      : DSolve
Description : Differential equation solver
Copyright   : (c) Tanja Malič, Grega Gašperšič, 2015
License     : BDS3
Stability   : experimental 

DSolve module contains functions for solving differential equations.
-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Numeric.DSolve where
import Numeric.LazySplines
import Data.List
import Control.Arrow

-- | The function @dsolve@ solves the first order differential equation @f@ with the initial or boundary condition @x0@.
dsolve :: (Spline -> Spline) -> 
			Spline -> Spline
dsolve f x0 = x
	where
		x' = f x
		x = x0 ++ (liftS (head (snd (head x0))) + integrateSpline x')

-- | The function @dsolveWithEps@ solves the first order differential equation @f@ with the initial or boundary condition
--   @x0@ of duration @eps@.
dsolveWithEps :: (Spline -> Spline) -> 
			Double -> Double -> Spline
dsolveWithEps f x0 eps = dsolve f (liftSWithDuration eps x0) 

-- | The function @dsolveWithTrim@ solves the first order differential equation @f@ with the initial or boundary condition
--   @x0@. All polynomial coefficients are limited to @trim@.
dsolveWithTrim :: (Spline -> Spline) -> 
			Spline -> Int -> Spline
dsolveWithTrim f x0 trim = x
	where 
		x' = f x
			`trimmingTo` trim
		x = x0 ++ integrateSpline x'
		
-- | The function @dsolveWithEpsAndTrim@ solves the first order differential equation @f@ with the initial or boundary 
--   condition @x0@ of duration @eps@. All polynomial coefficients are limited to @trim@.
dsolveWithEpsAndTrim :: (Spline -> Spline) -> 
			Double -> Double -> Int -> Spline
dsolveWithEpsAndTrim f x0 eps trim = dsolveWithTrim f (liftSWithDuration eps x0)  trim

-- | The function @dsolveWithHigherOrder@ solves the first order differential equation @f@ where the initial or boundary 
--   condition is the @n@-th polynomial of the solution of @f@ with the initial or boundary condition @x0@.
dsolveWithHigherOrder :: (Spline -> Spline) -> 
			Spline -> Int -> Spline
dsolveWithHigherOrder f x0 n = x
	where
		dx = take (n + 1) (dsolve f x0)
		dx0 = [(last dx)]
		q = (head (snd (head x0))) - (head (snd (head dx0)))
		x = (drop n (dsolve f x0)) + (liftS q)

-- | The function @dsolve2@ solves the second order differential equation @f@ with the initial or boundary condition @x0@ 
--   and @x0'@.
dsolve2 :: (Spline -> Spline -> Spline) -> 
			Spline -> Spline -> Spline
dsolve2 f x0 x0' = x
	where
		x'' = f x x'
		x' = x0' ++ integrateSpline x''
		x = x0 ++ integrateSpline x'

-- | The function @dsolve2WithEps@ solves the first order differential equation @f@ with the initial or boundary condition
--   @x0@ of duration @eps@ and @x0'@ of duration @eps'@.
dsolve2WithEps :: (Spline -> Spline -> Spline) -> 
			Double -> Double -> 
			Double -> Double -> Spline
dsolve2WithEps f x0 eps x0' eps' = dsolve2 f (liftSWithDuration eps x0) (liftSWithDuration eps' x0') 

-- | The function @dsolve2WithTrim@ solves the second order differential equation @f@ with the initial or boundary condition 
--   @x0@ and @x0'@. All polynomial coefficients of the first order are limited to @trim@ and all polynomial coefficients of
--   the first order are limited to @trim'@.
dsolve2WithTrim :: (Spline -> Spline -> Spline) -> 
			Spline -> Int -> 
			Spline -> Int -> Spline
dsolve2WithTrim f x0 trim x0' trim' = x
	where
		x'' = f x x'
		x' = x0' ++ integrateSpline x''
			`trimmingTo` trim'
		x = x0 ++ integrateSpline x'
			`trimmingTo` trim

-- | The function @dsolve2WithEps@ solves the first order differential equation @f@ with the initial or boundary condition @x0@
--   of duration @eps@ and @x0'@ of duration @eps'@.  All polynomial coefficients of the first order are limited to @trim@ and 
--   all polynomial coefficients of the first order are limited to @trim'@.
dsolve2WithEpsAndTrim :: (Spline -> Spline -> Spline) -> 
			Double -> Double -> Int -> 
			Double -> Double -> Int -> Spline
dsolve2WithEpsAndTrim f x0 eps trim x0' eps' trim' = dsolve2WithTrim f (liftSWithDuration eps x0) trim (liftSWithDuration eps' x0') trim'

-- | The function @dsolve2WithHigherOrder@ solves the first order differential equation @f@ where the initial or boundary 
--   condition is the @n@-th polynomial of the solution of @f@ with the initial or boundary condition @x0@ and @x0'@.
dsolve2WithHigherOrder
  :: (Spline -> Spline -> Spline)
     -> Spline -> Spline -> Int -> Spline
dsolve2WithHigherOrder f x0 x0' n = x
	where
		dx = take (n + 1) (dsolve2 f x0 x0')
		dx0 = [(last dx)]
		q = (head (snd (head x0))) - (head (snd (head dx0)))
		x = (drop n (dsolve2 f x0 x0')) + (liftS q)