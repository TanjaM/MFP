module Numeric.LazySplines.Examples where
import Numeric.LazySplines


duckDeathAtAge = [ (10, [0]),
                   (10, [0.2]),
                   (15, [0.2, 0.01]) ]
initialLife = [(1, [0])]

survival :: Spline
survival = 1 - duckLife

duckLife = initialLife ++ integrateSpline duckLife'

duckLife' = duckDeathAtAge * survival


initialSpring = [(0.01, [-0.5, 1, 18])]

spring'' = -36 * spring
    `trimmingTo` 15

spring :: Spline
spring = initialSpring ++
           liftS (initialSpring `at` 0.01) +
           integrateSpline (integrateSpline spring'')

spring2'' = -36 * spring2
    `trimmingTo` 15
    `extrapForward` 0.01
spring2   = initialSpring ++
              liftS (initialSpring `at` 0.01) +
              integrateSpline
                 (integrateSpline spring2'')


flameDefect f = deriveSpline f - (f ^ 2) * (1 - f)

initialFlame = [(1, [0.01, 9.9e-5])]

flame' = flame^2 * (1 - flame)
    `trimmingTo` 15
    `extrapForward` 1

flame :: Spline
flame  = initialFlame ++
           (liftS (initialFlame `at` 1) +
           integrateSpline flame')


flamePred :: SplinePredicate
flamePred t d f  = v' - v^2 * (1 - v)
  where v  = f `at` d
        v' = diff f `at` d

flame2' = flame2^2 * (1 - flame2)
    `trimmingTo` 15
    `extrapForward` 1

flame2 :: Spline
flame2 = initialFlame ++
           liftS (initialFlame `at` 1) +
           integrateSpline flame2'
    `satisfying` (0.00001, flamePred)

flame3' = flame3^2 * (1 - flame3)
    `trimmingTo` 15
    `extrapForward` 1

flame3 :: Spline
flame3 = initialFlame ++
           liftS (initialFlame `at` 1) +
           integrateSpline flame3'
    `splitWhen`  (0.00001, 0.125, flamePred)
    `satisfying` (0.00001, flamePred)

flame4' = flame4^2 * (1 - flame4)
    `trimmingTo` 15
    `extrapForward` 1

flame4 :: Spline
flame4 = initialFlame ++
           liftS (initialFlame `at` 1) +
           integrateSpline flame4'
    `splitWhen`  (0.00001, 0.125, flamePred)
    `satisfying` (0.00001, flamePred)
    `extendWhen` (0.00001, 8, flamePred)


flame5' = flame5^2 * (1 - flame5)
    `extrapForward` 1

flame5 :: Spline
flame5 = initialFlame ++
           liftS (initialFlame `at` 1) +
           integrateSpline flame5'
    `trimSmart`  flamePred
    `splitWhen`  (0.00001, 0.125, flamePred)
    `satisfying` (0.00001, flamePred)
    `extendWhen` (0.00001, 8, flamePred)
