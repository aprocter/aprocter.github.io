module Fibonacci where

type Re = ReactT W8 Bit I

w8_zero = W8 (Zero,Zero,Zero,Zero,Zero,Zero,Zero,Zero)
w8_one = W8 (Zero,Zero,Zero,Zero,Zero,Zero,Zero,One)

machine_main :: Re ()
machine_main = signal w8_zero >>= \ _ ->
                 machine_fib (w8_zero,w8_one)

               {-stepRe w8_zero (\ _ -> return (w8_zero,w8_one)) >>=
                   machine_fib-}
  
machine_fib :: (W8,W8) -> Re ()
machine_fib (r1,r2) = signal r1 >>= \ sw ->
                      case sw of
                        One  -> machine_fib (r2,r1+r2)
                        Zero -> machine_fib (r1,r2)

                     {-stepRe r1 (\ sw -> case sw of
                                           One  -> return (r2,r1+r2)
                                           Zero -> return (r1,r2)) >>=
                        machine_fib-}
