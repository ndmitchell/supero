{-
Compute digits of e
Due to John Hughes, Aug 2001
-}

module Test.Nofib.Digits_of_e2(test) where

#include "Include.h"

{-
Here's a way to compute all the digits of e. We use the series

   e = 2  +  1  +  1  +  1  +  ...
             --    --    --  
             2!    3!    4!

which we can think of as representing e as 2.11111... in a strange
number system with a varying base. In this number system, the fraction
0.abcd... represents

             a  +  b  +  c  +  d  +  ...
             --    --    --    --
             2!    3!    4!    5!

To convert such a fraction to decimal, we multiply by 10, take the
integer part for the next digit, and continue with the fractional
part. Multiplying by 10 is easy: we just multiply each "digit" by 10,
and then propagate carries.

The hard part is knowing how far carries might propagate: since we
carry leftwards in an infinite expansion, we must be careful to avoid
needing to inspect the entire fraction in order to decide on the first
carry. But each fraction we work with is less than one, so after
multiplying by 10, it is less than 10. The "carry out" from each digit
can be at most 9, therefore. So if a carry of 9 from the next digit
would not affect the carry out from the current one, then that carry
out can be emitted immediately. Since the base soon becomes much
larger than 10, then this is likely to happen quickly. No doubt there
are much better ways than this of solving the problem, but this one
works.
-}

carryPropagate base ds = case ds of
    (d:ds) ->
        let carryguess = d `div` base
            remainder = d `mod` base
            nextcarry_fraction = carryPropagate (base+1) ds
            nextcarry = head nextcarry_fraction
            fraction = tail nextcarry_fraction
            dCorrected = d + nextcarry
        in case carryguess == (d+9) `div` base of
              True -> carryguess : (remainder+nextcarry) : fraction
              False -> (dCorrected `div` base) : (dCorrected `mod` base) : fraction
    [] -> error "carryPropagate"

root :: Int -> String
root i = take i $ (('2':[])++) $ 
    tail . concat $
    map (show.head) $
    iterate (carryPropagate 2 . map (10*) . tail) $
    2:repeat 1

#if MAIN
test = (\i -> root (i :: Int), 1000 :: Int)
#endif
