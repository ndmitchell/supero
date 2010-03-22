{-# OPTIONS_GHC -O2 #-}
module Main(main) where
import Complex

main = print $ root ( 80000 :: Int)

{-# INLINE f'1 #-}
f'1 :: Int -> Complex Double
f'1 n = mkPolar 1 ((2*pi)/fromIntegral n) ^ n

round'1 = round
realPart'1 = realPart
compAdd'2 = (+) :: Complex Double -> Complex Double -> Complex Double
intAdd'2 = (+) :: Int -> Int -> Int
intGt'2 = (>) :: Int -> Int -> Bool

root = (\ v1 -> let v2 = (f1 v1) in v2)
f1
  = (\ v1 ->
       let v2 = (round'1 v3)
           v3 = (f2 v1)
         in v2)
f2 = (\ v1 -> let v2 = (f3 v1) in v2)
f3
  = (\ v1 ->
       let v2 = (realPart'1 v3)
           v3 = (f4 v1)
         in v2)
f4
  = (\ v1 ->
       let v5 = (intGt'2 v6 v1)
           v2 = (f5 v1 v5)
           v6 = (f10)
         in v2)
f5
  = (\ v1 v2 ->
       case v2 of
           True -> (f6)
           False -> (f7 v1))
f6 = (let _flat = (0) in _flat)
f7
  = (\ v1 ->
       let v7 = (intGt'2 v8 v1)
           v2 = (f8 v1 v7 v8)
           v8 = (f22)
         in v2)
f8
  = (\ v1 v2 v3 ->
       case v2 of
           True -> (f9)
           False -> (f11 v1 v3))
f9
  = (let v1 = (f'1 v2)
         v2 = (f10)
       in v1)
f10 = (let _flat = (1) in _flat)
f11
  = (\ v1 v2 ->
       let v9 = (intGt'2 v10 v1)
           v3 = (f12 v1 v10 v2 v9)
           v10 = (f21 v2)
         in v3)
f12
  = (\ v1 v2 v3 v4 ->
       case v4 of
           True -> (f13 v3)
           False -> (f15 v1 v2 v3))
f13
  = (\ v1 ->
       let v2 = (compAdd'2 v3 v4)
           v3 = (f14 v1)
           v4 = (f9)
         in v2)
f14 = (\ v1 -> let _flat = (f'1 v1) in _flat)
f15
  = (\ v1 v2 v3 ->
       let v4 = (f16 v1 v11 v2 v3)
           v11 = (f9)
         in v4)
f16
  = (\ v1 v2 v3 v4 ->
       let v11 = (intGt'2 v12 v1)
           v5 = (f17 v1 v11 v12 v2 v3 v4)
           v12 = (f21 v3)
         in v5)
f17
  = (\ v1 v2 v3 v4 v5 v6 ->
       case v2 of
           True -> (f18 v4 v5 v6)
           False -> (f20 v1 v3 v4 v5 v6))
f18
  = (\ v1 v2 v3 ->
       let v4 = (compAdd'2 v5 v6)
           v5 = (f14 v2)
           v6 = (f19 v1 v3)
         in v4)
f19
  = (\ v1 v2 ->
       let v3 = (compAdd'2 v4 v1)
           v4 = (f14 v2)
         in v3)
f20
  = (\ v1 v2 v3 v4 v5 ->
       let v6 = (f16 v1 v13 v2 v4)
           v13 = (f19 v3 v5)
         in v6)
f21
  = (\ v1 ->
       let v2 = (intAdd'2 v1 v3)
           v3 = (f10)
         in v2)
f22
  = (let v1 = (intAdd'2 v2 v3)
         v2 = (f10)
         v3 = (f10)
       in v1)

