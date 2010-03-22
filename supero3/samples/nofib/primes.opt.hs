{-# OPTIONS_GHC -O2 #-}
module Main(main) where

main = print (root (4000 :: Int) :: Int)


addInt'2 = (+)
eqInt'2 = (==)
neqInt'2 = (/=)
modInt'2 = mod
subInt'2 = (-)
error'1 = error

root
  = (\ v1 ->
       let v3 = (eqInt'2 v1 v6)
           v2 = (f1 v1 v3)
           v6 = (f20)
         in v2)
f1
  = (\ v1 v2 ->
       case v2 of
           True -> (f2)
           False -> (f3 v1))
f2 = (let _flat = (2) in _flat)
f3
  = (\ v1 ->
       let v3 = (eqInt'2 v6 v7)
           v2 = (f4 v3 v6)
           v6 = (f24 v1)
           v7 = (f20)
         in v2)
f4
  = (\ v1 v2 ->
       let v3 = (f5 v1 v2 v6)
           v6 = (f26)
         in v3)
f5
  = (\ v1 v2 v3 ->
       case v1 of
           True -> (f6 v3)
           False -> (f11 v2 v3))
f6
  = (\ v1 ->
       case v1 of
           ([]) -> (f7)
           (:) v4 v5 -> (f10 v4))
f7 = (let v1 = (f8) in v1)
f8
  = (let v1 = (error'1 v2)
         v2 = (f9)
       in v1)
f9 = (let _flat = (("head")) in _flat)
f10 = (\ v1 -> let in v1)
f11
  = (\ v1 v2 ->
       let v4 = (eqInt'2 v7 v8)
           v3 = (f12 v2 v4 v7)
           v7 = (f24 v1)
           v8 = (f20)
         in v3)
f12
  = (\ v1 v2 v3 ->
       let v4 = (f5 v2 v3 v7)
           v7 = (f13 v1)
         in v4)
f13
  = (\ v1 ->
       case v1 of
           (:) v2 v3 -> (f14 v2 v3)
           ([]) -> (f21))
f14
  = (\ v1 v2 ->
       case v2 of
           ([]) -> (f15)
           (:) v5 v6 -> (f16 v1 v5 v6))
f15 = (let _flat = (([])) in _flat)
f16
  = (\ v1 v2 v3 ->
       let v5 = (neqInt'2 v7 v8)
           v4 = (f17 v1 v2 v3 v5)
           v7 = (f19 v1 v2)
           v8 = (f20)
         in v4)
f17
  = (\ v1 v2 v3 v4 ->
       case v4 of
           True -> (f18 v1 v2 v3)
           False -> (f14 v1 v3))
f18
  = (\ v1 v2 v3 ->
       let v4 = ((:) v2 v5)
           v5 = (f14 v1 v3)
         in v4)
f19 = (\ v1 v2 -> let _flat = (modInt'2 v2 v1) in _flat)
f20 = (let _flat = (0) in _flat)
f21 = (let v1 = (f22) in v1)
f22
  = (let v1 = (error'1 v2)
         v2 = (f23)
       in v1)
f23 = (let _flat = (("the_filter")) in _flat)
f24
  = (\ v1 ->
       let v2 = (subInt'2 v1 v3)
           v3 = (f25)
         in v2)
f25 = (let _flat = (1) in _flat)
f26
  = (let v2 = (neqInt'2 v6 v7)
         v1 = (f27 v2 v3)
         v3 = (f33)
         v6 = (f31 v3)
         v7 = (f20)
       in v1)
f27
  = (\ v1 v2 ->
       case v1 of
           True -> (f28 v2)
           False -> (f29 v2))
f28
  = (\ v1 ->
       let v2 = ((:) v1 v3)
           v3 = (f29 v1)
         in v2)
f29
  = (\ v1 ->
       let v3 = (neqInt'2 v7 v8)
           v2 = (f27 v3 v4)
           v4 = (f30 v1)
           v7 = (f31 v4)
           v8 = (f20)
         in v2)
f30
  = (\ v1 ->
       let v2 = (addInt'2 v1 v3)
           v3 = (f25)
         in v2)
f31
  = (\ v1 ->
       let v2 = (modInt'2 v1 v3)
           v3 = (f32)
         in v2)
f32 = (let _flat = (2) in _flat)
f33
  = (let v1 = (addInt'2 v2 v3)
         v2 = (f32)
         v3 = (f25)
       in v1)

