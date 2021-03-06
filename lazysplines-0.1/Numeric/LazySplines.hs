{-|
Module      : LazySplines
Description : Lazy splines implementation.
Copyright   : (c) Tanja Malič, Grega Gašperšič, 2015
License     : BDS3
Stability   : experimental 

LazySplines module is an extended implementation of the Numeric.LazySplines module by Gershom Bazerman.
-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Numeric.LazySplines where

import Data.List
import Control.Arrow

-- | Something that can be sampled.
class Sampleable a where
    at :: a -> Double -> Double

-- | Definition of poly.
type Poly = [Double]

-- Horner's scheme for polynomial evaluation.
instance Sampleable Poly where
    at x v = foldr (\c val -> c + v * val) 0 x

-- | Sampleable evaluation of a list of polynomials.
instance Sampleable [Poly] where
    at x v = poly `at` frac
      where
        poly = x !! int
        (int,frac) = properFraction v

-- | Definition of a poly segment.
type PolySegment = (Double, Poly)

-- | Definition of spline.
type Spline = [PolySegment]

-- | Definition of a spline predicate.
type SplinePredicate =
    Double -> Double -> Poly -> Double

-- | The function @duration@ calculates the duration of a spline. The duration is equal to the sum of all of the 
--   duration of poly segments.
duration :: Spline -> Double
duration = sum . map fst

maxDuration = 10000  -- an arbitrary limit

-- | The function @liftS@ creates a spline with one poly segment with the @maxDuration@ and @x@ polynomial coefficient.
liftS :: Double -> Spline
liftS x = liftSWithDuration maxDuration x

-- | The function @liftSWithDuration@ creates a spline with one poly segment with the specified @duration@ and @x@ 
--   polynomial coefficient.
liftSWithDuration :: Double -> Double -> Spline
liftSWithDuration duration x = [(duration,[x])]

-- | Sampleable evaluation of a polynomial segment.
instance Sampleable PolySegment where
  at (_,poly) pt = poly `at` pt

-- | Sampleable evaluation of a spline.
instance Sampleable Spline where
  -- assume pt >= 0
  at spline pt = go spline pt where
    go ((dur,poly):xs) v
        | v <= dur  = poly `at` v
        | otherwise = go xs (v - dur)
    go [] _ = error $
        "Sampling spline of out bounds " ++
        show spline ++ " at: " ++ show pt

-- | The function @deriveSpline@ calculates the @diff@ of all of the poly segments in a spline.
deriveSpline :: Spline -> Spline
deriveSpline = map (second diff)

-- | The function @integrateSplineWithDuration@ integrates the specified spline. The integration process starts in the 
--   specified @startDuration@.
integrateSplineWithDuration :: Double -> Spline -> Spline
integrateSplineWithDuration startDuration =
    snd . mapAccumL go startDuration . map (second integ)
  where
    go :: Double -> PolySegment -> (Double, PolySegment)
    go acc (dur,poly) = (v, seg)
      where v = acc + poly `at` dur
            seg = (dur, realToFrac acc + poly)

-- | The function @integrateSplineWithDuration@ integrates the specified spline. The integration process starts in 0.
integrateSpline :: Spline -> Spline
integrateSpline = integrateSplineWithDuration 0

-- | The function @sumSpline@ sums all of the coefficients of the specified @spline@ and returns a spline of length 0 or 1.
sumSpline :: Spline -> Spline
sumSpline spline = case spline of
	[] -> []
	((ds, ps):st) -> helper st [(ds, ps)]
	where
		helper [] acc = acc
		helper ((dx, px):xt) acc = helper xt (inSpline2 (+) [(dx, px)] acc)

-- | The function @inSpline2@ applies the @op@ to the @f@ and @g@ splines.
inSpline2 :: (Poly -> Poly -> Poly) ->
             Spline -> Spline -> Spline
inSpline2 op ((xd,x):xs) ((yd,y):ys)
    | xd == yd  = (xd, v) : inSpline2 op xs ys
    | xd <  yd  = (xd, v) : inSpline2 op xs (y':ys)
    | otherwise = (yd, v) : inSpline2 op (x':xs) ys
  where
    v = x `op` y
    x' = splitPoly yd (xd,x)
    y' = splitPoly xd (yd,y)
    splitPoly d (dur,poly) = (dur - d, shiftBy d poly)
inSpline2 _ _ _ = []

-- | The function @shiftBy@ shifts a specified @poly@ for a amount @d@.
shiftBy :: Double -> Poly -> Poly
shiftBy d poly = poly # [d,1]

-- | Num evaluation of a spline.
instance Num Spline where
    fromInteger = liftS . fromInteger
    negate = map (second negate)
    (+)    = inSpline2 (+)
    (*)    = inSpline2 (*)

-- | Fractional evaluation of a spline.
instance Fractional Spline where
   fromRational = liftS . fromRational

-- | The function @mapSpline@ applies a function to the specified spline.
mapSpline :: Bool ->
             (Double -> Double -> Poly -> Poly) ->
             Spline ->
             Spline
mapSpline _          _ []         = []
mapSpline matchFirst f (seg:segs) =
    -- leave first segment unchanged
    seg :
    (snd $ mapAccumL go (dur0, seg `at` dur0) segs)
  where
    dur0 = fst seg

    go (totalDur, lastVal) (dur, poly) =
        ((totalDur + dur, fnc' `at` dur), fnc')
      where
        fnc' = (dur, poly')

        poly'
           -- modify translated segment
         | matchFirst = f totalDur dur $
                        match lastVal poly
           -- translate modified segment
         | otherwise  = matchScale lastVal dur $
                        f totalDur dur poly

-- | The function @match@ replace 0-degree coefficient of a poly with the @lastVal@.
match lastVal (_:xs) = lastVal:xs
match lastVal x      = [lastVal]

-- | The function @matchScale@ alters the first point to @v@ and preserves the point at @dur@.
matchScale v dur poly@(x:xs) = v : map (* scale) xs
           where height = poly `at` (dur - x)
                 diff   = x - v
                 scale = height / (height - diff)
matchScale v _ x = [v]

-- | The function @trimmingTo@ trimms every poly in the spline to the specified power.
infixl 1 `trimmingTo`
trimmingTo :: Spline -> Int -> Spline
trimmingTo spline power =
    mapSpline True go spline
  where
    go _ _ s = take power s

-- | The function @takeToDuration@ takes the spline to the specified duration.
infixl 1 `takeToDuration`
takeToDuration :: Spline -> Double -> Spline
takeToDuration spline duration = helper spline duration [] 0
	where
		helper [] _ acc _ = reverse acc
		helper ((ds, ps):fs) maxduration acc accdur = case compare maxduration (ds + accdur) of
			LT -> if maxduration - (ds + accdur) == 0
				then reverse acc
				else reverse ((maxduration - (ds + accdur), ps):acc) 
			GT -> helper fs maxduration ((ds, ps):acc) (ds + accdur)
			EQ -> reverse ((ds, ps):acc)
		
-- | The function @extrapForward@ shifts every poly in the spline for the specified delta.
infixl 1 `extrapForward`
extrapForward :: Spline -> Double -> Spline
extrapForward spline delta =
    mapSpline False go spline
  where
    go _ _ s = shiftBy delta s

-- | The function @scaleRest@ scales all of the values of the poly with @c@, except for the first one.
scaleRest :: Poly -> Double -> Poly
scaleRest (x:xs) c = x : map (* c) xs

infixl 1 `satisfying`
satisfying :: Spline ->
              (Double, SplinePredicate) ->
              Spline
satisfying spline (tol, p) =
    mapSpline True go spline
  where
    go t d fnc = findValue tol (p t d) $
                 scaleRest fnc

infixl 1 `splitWhen`
splitWhen :: Spline ->
             (Double, Double, SplinePredicate) ->
             Spline
splitWhen spline (tol, minsize, p) =
    go 0 spline
  where
    go _ [] = []
    go t (f@(dur,poly):fs)
        | doSplit   = go t (f' : f'' : fs)
        | otherwise = f : go t' fs
      where

        -- split if predicate is not satisfied
        -- and duration is not below minsize
        doSplit = dur > minsize &&
                  abs (p t dur poly) > tol

        -- two segments, each with half
        -- the duration of the original
        dur' = dur / 2
        f'   = (dur', poly)
        f''  = (dur', shiftBy dur' poly)

        t' = t + dur

infixl 1 `extendWhen`
extendWhen :: Spline ->
              (Double, Double, SplinePredicate) ->
              Spline
extendWhen spline (tol,maxlen,p) =
    go (spline `at` 0, 0) 0 spline
  where
    go _ _ [] = []
    go (lastVal, time) chop ((oldDur,oldPoly):fs)

          -- This segment has been subsumed
          -- by previous extensions,
          -- disregard it.
        | dur <= 0  =
            go (lastVal, time) (negate dur) fs

          -- Attempt to extend this segment
        | otherwise =
            (dur', poly') :
            go (lastVal', time') chop' fs
      where

        -- The segment as chopped
        -- to reflect prior extensions
        poly = shiftBy chop oldPoly
        dur  = oldDur - chop

        chkPred d =
            (abs $ p time d poly) < tol

        -- greatest permissible duration
        -- that satisfies the predicate
        dur' = lastDef dur .
               takeWhile chkPred .
               -- list of possible durations
               takeWhile (<= maxlen) $
               iterate (* 2) dur

        chop' = dur' - dur
        time' = time + dur'
        lastVal' = poly `at` dur'
        poly' = matchScale lastVal dur' poly

lastDef def []       = def
lastDef _ [x]        = x
lastDef def (x : xs) = lastDef def xs


infixl 1 `trimSmart`
trimSmart :: Spline ->
             SplinePredicate ->
             Spline
trimSmart spline p =
    mapSpline True go spline
  where
    go t dur poly =
        headDef poly .
        map fst .
        dropWhile chkPred $
        -- pair adjacent candidate
        -- polynomials
        zip polys (drop 1 polys)
      where
        -- possible trimmed polynomials
        polys = drop 2 . inits $  poly

        chkPred (x,x') = p t dur x >
                         p t dur x'

headDef def []  = def
headDef _ (x:_) = x


-- Adopted from M. Douglas McIlroy., Power series, power serious.
-- http://www.cs.dartmouth.edu/~doug/powser.html

-- | Num evaluation of a poly.
instance Num Poly where
   fromInteger c = [fromInteger c]
	
   negate fs = map negate fs

   (f:ft) + (g:gt) = f+g : ft+gt
   fs + [] = fs
   [] + gs = gs

   (f:ft) * gs@(g:gt) =
       dropZeros $ f*g : ft*gs + [f]*gt
   _ * _ = []

-- | Fractional evaluation of a poly.
instance Fractional Poly where
   fromRational c = [fromRational c]

   (0:ft) / gs@(0:gt) = ft/gt
   (0:ft) / gs@(g:gt) = 0 : ft/gs
   (f:ft) / gs@(g:gt) = f/g : (ft-[f/g]*gt)/gs
   [] / (0:gt) = []/gt
   [] / (g:gt) = []
   _ / _ = error "improper polynomial division"

   
-- | Spline composition operator 
infixl 1 `splineComposition`
-- Spline composition
splineComposition :: Spline -> Spline -> Spline
splineComposition a b = helper a b [] 0 0
	where
		helper [] g acc durf durg = (reverse acc) ++ g
		helper f [] acc durf durg = (reverse acc) ++ f
		helper ((df, pf):ft) ((dg, pg):gt) acc durf durg = case compare (durf + df) (durg + dg) of 
			LT -> if dg - df == 0
				  then helper ft gt ((df, pf # pg):acc) (durf + df) durg
				  else helper ft ((dg - df, pg):gt) ((df, pf # pg):acc) (durf + df) (durg + dg - df)
			GT -> if df - dg == 0
				  then helper ft gt ((dg, pf # pg):acc) durf (durg + dg)
				  else helper ((df - dg, pf):ft) gt ((dg, pf # pg):acc) (durf + df - dg) (durg + dg)
			EQ -> helper ft gt (((min df dg), pf # pg):acc) (durf + df) (durg + dg)


-- | Polynomial composition operator 
infixr 9 #
-- Polynomial composition
(f:ft) # gs@(0:gt) = f : gt*(ft#gs)
(f:ft) # gs@(g:gt) = [f] + gs*(ft#gs)
[] # _ = []
(f:_) # [] = [f]

-- Polynomial integration
integ fs = dropZeros $
           0 : zipWith (/) fs (countFrom 1)

-- Polynomial differentiation
diff (_:ft) = zipWith (*) ft (countFrom 1)
diff _ = [0]

countFrom n = n : countFrom (n+1)

dropZeros = foldr f [] where
  f elem acc | elem == 0 && null acc = acc
             | otherwise             = elem:acc


-- Code for root finding

newton :: Double -> (Double -> Double) -> [Double]
newton initial f = iterate go initial where
    go x = x - (approx / df)
      where approx = f x
            df = (f (x + delta) - approx) / delta
            delta = 0.001
pickValue :: Double -> (a -> Double) -> [a] -> a
pickValue tol f =
    -- give up after 1000 tries
    foldr go err . take 1000
  where
    go x x' = if abs (f x) <= tol then x else x'
    err = error "can't converge"
findValue :: Double ->
             (a -> Double) ->
             (Double -> a) ->
             a
findValue tol p fnc =
    -- we use an initial guess of 1
    pickValue tol p $ map fnc $ newton 1 (p . fnc)
