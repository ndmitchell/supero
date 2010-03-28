
import System


-- code of unknown provenance (partain 95/01/25)

tak :: Int -> Int -> Int -> Int

tak x y z = if not(y < x) then z
       else tak (tak (x-1) y z)
		(tak (y-1) z x)
		(tak (z-1) x y)

root x y z = tak x y z


#if MAIN
main = print $ root (24::Int) (16::Int) (8::Int)
subInt'2 = (-) :: Int -> Int -> Int
ltInt'2 = (<) :: Int -> Int -> Bool
#endif

#if SUPERO
(-) = subInt'2
(<) = ltInt'2
not x = case x of
    True -> False
    False -> True
#endif


