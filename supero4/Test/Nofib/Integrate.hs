
module Test.Nofib.Integrate(test) where

integrate1D :: Double -> Double -> (Double->Double) -> Double
integrate1D l u f =
  let  d = (u-l)/8.0 in
     d * sum
        ((f l)*0.5 :
        f (l+d) :
        f (l+(2.0*d)) :
        f (l+(3.0*d)) :
        f (l+(4.0*d)) :
        f (u-(3.0*d)) :
        f (u-(2.0*d)) :
        f (u-d) :
        (f u)*0.5 : [])

integrate2D l1 u1 l2 u2 f = integrate1D l2 u2 
				    (\y->integrate1D l1 u1 
						  (\x->f x y))

zark u v = integrate2D 0.0 u 0.0 v (\x->(\y->x*y))

-- type signature required for compilers lacking the monomorphism restriction

etotal n =
    let ints = enumFrom 1.0 
        zarks = zipWith zark ints (map (2.0*) ints)
        rtotals = head zarks : zipWith (+) (tail zarks) rtotals

        is = map (pow 4) ints
        itotals = head is : zipWith (+) (tail is) itotals
    in sum $ take n $ map (pow 2) (zipWith (-) rtotals itotals)

-- The (analytical) result should be zero
root n = etotal n

pow x y = y ^ x

#if MAIN
test = (root, 5000)
#endif
