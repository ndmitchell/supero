{-# OPTIONS_GHC -O2 #-}
module Main(main) where

main = print $ root (1000 :: Int)


addInt'2 = (+)
eqInt'2 = (==)
neqInt'2 = (/=) :: Int -> Int -> Bool
modInt'2 = mod
showInt'1 = show :: Int -> String
mulInt'2 = (*)
divInt'2 = div
subInt'2 = (-)
error'1 = error

root
  = (\ v1 ->
       let v3 = (eqInt'2 v1 v4)
           v2 = (f1 v1 v3)
           v4 = (f78)
         in v2)
f1
  = (\ v1 v2 ->
       case v2 of
           True -> (f2)
           False -> (f3 v1))
f2 = (let _flat = (([])) in _flat)
f3
  = (\ v1 ->
       let v2 = ((:) v3 v4)
           v3 = (f4)
           v4 = (f5 v1)
         in v2)
f4 = (let _flat = ('2') in _flat)
f5
  = (\ v1 ->
       let v3 = (eqInt'2 v5 v6)
           v2 = (f6 v3 v5)
           v5 = (f77 v1)
           v6 = (f78)
         in v2)
f6
  = (\ v1 v2 ->
       case v1 of
           True -> (f2)
           False -> (f7 v2))
f7
  = (\ v1 ->
       let v2 = (f8 v1 v21)
           v21 = (f93)
         in v2)
f8
  = (\ v1 v2 ->
       let v6 = (showInt'1 v8)
           v3 = (f9 v1 v14 v2 v6)
           v8 = (f91 v14)
           v14 = (f92)
         in v3)
f9
  = (\ v1 v2 v3 v4 ->
       case v4 of
           ([]) -> (f10 v1 v2 v3)
           (:) v35 v36 -> (f82 v1 v2 v3 v36))
f10
  = (\ v1 v2 v3 ->
       let v4 = (f11 v1 v11 v3)
           v11 = (f81 v2 v3)
         in v4)
f11
  = (\ v1 v2 v3 ->
       let v7 = (showInt'1 v9)
           v4 = (f12 v1 v2 v3 v7)
           v9 = (f25 v2)
         in v4)
f12
  = (\ v1 v2 v3 v4 ->
       case v4 of
           ([]) -> (f13 v1 v2 v3)
           (:) v33 v34 -> (f62 v1 v2 v3 v34))
f13
  = (\ v1 v2 v3 ->
       let v4 = (f11 v1 v11 v3)
           v11 = (f14 v2 v3)
         in v4)
f14
  = (\ v1 v2 ->
       case v1 of
           ([]) -> (f15 v2)
           (:) v48 v49 -> (f17 v2 v49))
f15 = (\ v1 -> let v2 = (f16 v1) in v2)
f16
  = (\ v1 ->
       let v5 = (error'1 v6)
           v2 = (f17 v1 v5)
           v6 = (f33)
         in v2)
f17
  = (\ v1 v2 ->
       case v2 of
           (:) v40 v41 -> (f18 v1 v40 v41)
           ([]) -> (f46))
f18
  = (\ v1 v2 v3 ->
       let v5 = (eqInt'2 v6 v12)
           v4 = (f19 v1 v18 v3 v5 v6)
           v6 = (f59 v18)
           v12 = (f61 v18)
           v18 = (f56 v1 v2)
         in v4)
f19
  = (\ v1 v2 v3 v4 v5 ->
       case v4 of
           True -> (f20 v1 v2 v3 v5)
           False -> (f58 v1 v2 v3))
f20
  = (\ v1 v2 v3 v4 ->
       let v5 = ((:) v4 v6)
           v6 = (f21 v1 v2 v3)
         in v5)
f21
  = (\ v1 v2 v3 ->
       let v4 = ((:) v5 v6)
           v5 = (f22 v2 v9)
           v6 = (f30 v9)
           v9 = (f34 v1 v3)
         in v4)
f22
  = (\ v1 v2 ->
       let v3 = (addInt'2 v4 v5)
           v4 = (f23 v1)
           v5 = (f25 v2)
         in v3)
f23
  = (\ v1 ->
       let v2 = (modInt'2 v1 v3)
           v3 = (f24)
         in v2)
f24 = (let _flat = (2) in _flat)
f25
  = (\ v1 ->
       case v1 of
           ([]) -> (f26)
           (:) v4 v5 -> (f29 v4))
f26 = (let v1 = (f27) in v1)
f27
  = (let v1 = (error'1 v2)
         v2 = (f28)
       in v1)
f28 = (let _flat = (("head")) in _flat)
f29 = (\ v1 -> let in v1)
f30
  = (\ v1 ->
       case v1 of
           ([]) -> (f31)
           (:) v4 v5 -> (f29 v5))
f31 = (let v1 = (f32) in v1)
f32
  = (let v1 = (error'1 v2)
         v2 = (f33)
       in v1)
f33 = (let _flat = (("tail")) in _flat)
f34
  = (\ v1 v2 ->
       case v2 of
           (:) v43 v44 -> (f35 v1 v43 v44)
           ([]) -> (f46))
f35
  = (\ v1 v2 v3 ->
       let v4 = (f36 v1 v10 v2 v3)
           v10 = (f57)
         in v4)
f36
  = (\ v1 v2 v3 v4 ->
       let v6 = (eqInt'2 v7 v12)
           v5 = (f37 v1 v18 v2 v4 v6 v7)
           v7 = (f50 v18 v2)
           v12 = (f53 v18 v2)
           v18 = (f56 v1 v3)
         in v5)
f37
  = (\ v1 v2 v3 v4 v5 v6 ->
       case v5 of
           True -> (f38 v1 v2 v3 v4 v6)
           False -> (f49 v1 v2 v3 v4))
f38
  = (\ v1 v2 v3 v4 v5 ->
       let v6 = ((:) v5 v7)
           v7 = (f39 v1 v2 v3 v4)
         in v6)
f39
  = (\ v1 v2 v3 v4 ->
       let v5 = ((:) v6 v7)
           v6 = (f40 v10 v2 v3)
           v7 = (f30 v10)
           v10 = (f42 v1 v3 v4)
         in v5)
f40
  = (\ v1 v2 v3 ->
       let v4 = (addInt'2 v5 v6)
           v5 = (f41 v2 v3)
           v6 = (f25 v1)
         in v4)
f41 = (\ v1 v2 -> let _flat = (modInt'2 v1 v2) in _flat)
f42
  = (\ v1 v2 v3 ->
       case v3 of
           (:) v43 v44 -> (f43 v1 v2 v43 v44)
           ([]) -> (f46))
f43
  = (\ v1 v2 v3 v4 ->
       let v5 = (f36 v1 v11 v3 v4)
           v11 = (f44 v2)
         in v5)
f44
  = (\ v1 ->
       let v2 = (addInt'2 v1 v3)
           v3 = (f45)
         in v2)
f45 = (let _flat = (1) in _flat)
f46 = (let v1 = (f47) in v1)
f47
  = (let v1 = (error'1 v2)
         v2 = (f48)
       in v1)
f48 = (let _flat = (("carryPropagate")) in _flat)
f49
  = (\ v1 v2 v3 v4 ->
       let v5 = ((:) v6 v7)
           v6 = (f50 v11 v3)
           v7 = (f51 v11 v13 v3)
           v11 = (f52 v13 v2)
           v13 = (f42 v1 v3 v4)
         in v5)
f50 = (\ v1 v2 -> let _flat = (divInt'2 v1 v2) in _flat)
f51
  = (\ v1 v2 v3 ->
       let v4 = ((:) v5 v6)
           v5 = (f41 v1 v3)
           v6 = (f30 v2)
         in v4)
f52
  = (\ v1 v2 ->
       let v3 = (addInt'2 v2 v4)
           v4 = (f25 v1)
         in v3)
f53
  = (\ v1 v2 ->
       let v3 = (divInt'2 v4 v2)
           v4 = (f54 v1)
         in v3)
f54
  = (\ v1 ->
       let v2 = (addInt'2 v1 v3)
           v3 = (f55)
         in v2)
f55 = (let _flat = (9) in _flat)
f56 = (\ v1 v2 -> let _flat = (v1 v2) in _flat)
f57
  = (let v1 = (addInt'2 v2 v3)
         v2 = (f24)
         v3 = (f45)
       in v1)
f58
  = (\ v1 v2 v3 ->
       let v4 = ((:) v5 v6)
           v5 = (f59 v11)
           v6 = (f60 v11 v13)
           v11 = (f52 v13 v2)
           v13 = (f34 v1 v3)
         in v4)
f59
  = (\ v1 ->
       let v2 = (divInt'2 v1 v3)
           v3 = (f24)
         in v2)
f60
  = (\ v1 v2 ->
       let v3 = ((:) v4 v5)
           v4 = (f23 v1)
           v5 = (f30 v2)
         in v3)
f61
  = (\ v1 ->
       let v2 = (divInt'2 v3 v4)
           v3 = (f54 v1)
           v4 = (f24)
         in v2)
f62
  = (\ v1 v2 v3 v4 ->
       case v4 of
           ([]) -> (f63 v1 v2 v3)
           (:) v28 v29 -> (f66 v1 v2 v28 v29 v3))
f63
  = (\ v1 v2 v3 ->
       let v4 = (f64 v1 v10 v3)
           v10 = (f14 v2 v3)
         in v4)
f64
  = (\ v1 v2 v3 ->
       let v6 = (showInt'1 v8)
           v4 = (f65 v1 v2 v3 v6)
           v8 = (f25 v2)
         in v4)
f65
  = (\ v1 v2 v3 v4 ->
       case v4 of
           ([]) -> (f63 v1 v2 v3)
           (:) v28 v29 -> (f66 v1 v2 v28 v29 v3))
f66
  = (\ v1 v2 v3 v4 v5 ->
       let v6 = ((:) v3 v7)
           v7 = (f67 v1 v2 v4 v5)
         in v6)
f67
  = (\ v1 v2 v3 v4 ->
       let v6 = (eqInt'2 v8 v9)
           v5 = (f68 v2 v3 v4 v6 v8)
           v8 = (f77 v1)
           v9 = (f78)
         in v5)
f68
  = (\ v1 v2 v3 v4 v5 ->
       case v4 of
           True -> (f2)
           False -> (f69 v1 v2 v3 v5))
f69
  = (\ v1 v2 v3 v4 ->
       case v2 of
           ([]) -> (f70 v1 v3 v4)
           (:) v28 v29 -> (f79 v1 v28 v29 v3 v4))
f70
  = (\ v1 v2 v3 ->
       let v4 = (f71 v10 v2 v3)
           v10 = (f14 v1 v2)
         in v4)
f71
  = (\ v1 v2 v3 ->
       let v6 = (showInt'1 v8)
           v4 = (f72 v1 v2 v3 v6)
           v8 = (f25 v1)
         in v4)
f72
  = (\ v1 v2 v3 v4 ->
       case v4 of
           ([]) -> (f70 v1 v2 v3)
           (:) v28 v29 -> (f73 v1 v2 v28 v29 v3))
f73
  = (\ v1 v2 v3 v4 v5 ->
       let v6 = ((:) v3 v7)
           v7 = (f74 v1 v2 v4 v5)
         in v6)
f74
  = (\ v1 v2 v3 v4 ->
       let v6 = (eqInt'2 v8 v9)
           v5 = (f75 v1 v2 v3 v6 v8)
           v8 = (f77 v4)
           v9 = (f78)
         in v5)
f75
  = (\ v1 v2 v3 v4 v5 ->
       case v4 of
           True -> (f2)
           False -> (f76 v1 v2 v3 v5))
f76
  = (\ v1 v2 v3 v4 ->
       case v3 of
           ([]) -> (f70 v1 v2 v4)
           (:) v28 v29 -> (f73 v1 v2 v28 v29 v4))
f77
  = (\ v1 ->
       let v2 = (subInt'2 v1 v3)
           v3 = (f45)
         in v2)
f78 = (let _flat = (0) in _flat)
f79
  = (\ v1 v2 v3 v4 v5 ->
       let v6 = ((:) v2 v7)
           v7 = (f80 v1 v3 v4 v5)
         in v6)
f80
  = (\ v1 v2 v3 v4 ->
       let v6 = (eqInt'2 v8 v9)
           v5 = (f68 v1 v2 v3 v6 v8)
           v8 = (f77 v4)
           v9 = (f78)
         in v5)
f81
  = (\ v1 v2 ->
       case v1 of
           (:) v40 v41 -> (f18 v2 v40 v41)
           ([]) -> (f46))
f82
  = (\ v1 v2 v3 v4 ->
       case v4 of
           ([]) -> (f83 v1 v2 v3)
           (:) v30 v31 -> (f84 v1 v2 v3 v30 v31))
f83
  = (\ v1 v2 v3 ->
       let v4 = (f64 v1 v10 v3)
           v10 = (f81 v2 v3)
         in v4)
f84
  = (\ v1 v2 v3 v4 v5 ->
       let v6 = ((:) v4 v7)
           v7 = (f85 v1 v2 v3 v5)
         in v6)
f85
  = (\ v1 v2 v3 v4 ->
       let v6 = (eqInt'2 v8 v9)
           v5 = (f86 v2 v3 v4 v6 v8)
           v8 = (f77 v1)
           v9 = (f78)
         in v5)
f86
  = (\ v1 v2 v3 v4 v5 ->
       case v4 of
           True -> (f2)
           False -> (f87 v1 v2 v3 v5))
f87
  = (\ v1 v2 v3 v4 ->
       case v3 of
           ([]) -> (f88 v1 v2 v4)
           (:) v30 v31 -> (f89 v1 v2 v30 v31 v4))
f88
  = (\ v1 v2 v3 ->
       let v4 = (f71 v10 v2 v3)
           v10 = (f81 v1 v2)
         in v4)
f89
  = (\ v1 v2 v3 v4 v5 ->
       let v6 = ((:) v3 v7)
           v7 = (f90 v1 v2 v4 v5)
         in v6)
f90
  = (\ v1 v2 v3 v4 ->
       let v6 = (eqInt'2 v8 v9)
           v5 = (f86 v1 v2 v3 v6 v8)
           v8 = (f77 v4)
           v9 = (f78)
         in v5)
f91 = (\ v1 -> let _flat = (2) in _flat)
f92
  = (let v1 = ((:) v2 v3)
         v2 = (f45)
         v3 = (f92)
       in v1)
f93 = (let v1 = (f94) in v1)
f94
  = (let v1 = (mulInt'2 v2)
         v2 = (f95)
       in v1)
f95 = (let _flat = (10) in _flat)

