module Main where

import           Lib
import           Prelude hiding (and, div, map, mod, not, or, pow, sqrt, sum)

main :: IO ()
main = print (add 1 1)

zero :: Int -> Int -- const 0
zero = _N

-- const :: Int -> Int -> Int
-- const a _ = _R zero (\i past xs -> _Z past) a

-- const_arr :: Int -> [Int] -> Int
-- const_arr a _ = const a

zero_arr :: [Int] -> Int
zero_arr _ = zero 0

one_arr :: [Int] -> Int
one_arr _ = _Z (zero 0)

add :: Int -> Int -> Int -- +
add a = _R (const a) (\_ past _ -> _Z past) [0]

--1(a)
mul :: Int -> Int -> Int -- *
mul a = _R zero_arr (\_ past _ -> add past a) [0]

--1(b)
sub1 :: Int -> Int --
sub1 a = _R zero_arr (\i _ _ -> i) [] a

--1(c)
sub :: Int -> Int -> Int
sub a b = _R (const a) (\_ past _ -> sub1 past) [0] b

--  0 - False -> 0
--  0< - True -> 1

bool :: Int -> Int
bool cond = sub cond (sub1 cond)

iff :: Int -> Int -> Int -> Int
iff cond t f = _P (bool cond) [f,t]

not :: Int -> Int
not cond = iff cond 0 (_Z 0)

and :: Int -> Int -> Int
and cond1 cond2 = _P (add (bool cond1) (bool cond2)) [0,0,_Z 0]

or :: Int -> Int -> Int
or cond1 cond2 = _P (add (bool cond1) (bool cond2)) [0,(_Z 0),(_Z 0)]

-- a > b
greater :: Int -> Int -> Int
greater a b = bool (sub a b)

-- a < b
less :: Int -> Int -> Int
less a b = greater b a

-- a <= b - not(a > b)
leq :: Int -> Int -> Int
leq a b = not (greater a b)

-- (a >= b) == b <= a
geq :: Int -> Int -> Int
geq a b = leq b a

eq :: Int -> Int -> Int
eq a b = and (geq a b) (geq b a)

neq :: Int -> Int -> Int
neq a b = not(eq a b)

--1(d)
div :: Int -> Int -> Int
div a b = iff (neq b 0) (_R zero_arr (\_ past _ -> iff (leq (mul (_Z past) b) a) (_Z past) past) [] a) (undefined)

--1(e)
mod :: Int -> Int -> Int
mod a b = sub a (mul (div a b) b)

multiple :: Int -> Int -> Int
multiple a b = eq (mod a b) 0

deg :: Int -> Int -> Int
deg a b = _R (const 1) (\i past xs -> mul past a) [] b

--1(f)
plog :: Int -> Int -> Int
plog a b = iff (geq a 2) (_R zero_arr (\i past xs -> iff (multiple b (deg a (_Z past))) (_Z past) past) [] b) (undefined)

square :: Int -> Int
square a = mul a a

sqrt :: Int -> Int
sqrt a = _R zero_arr (\_ past _ -> iff (leq (square (_Z past)) a) (_Z past) past) [] a

--1(g)
is_prime :: Int -> Int
is_prime a = iff (less a 2) 0 (_R (const 1) (\i past _ -> (iff past (iff (less i 2) 1 (not (multiple a i))) 0)) [] (_Z (sqrt a)))

th_primes :: Int -> Int -> Int
th_primes i n = iff (is_prime i) (iff (eq n 1) i (th_primes (_Z i) (sub1 n))) (th_primes (_Z i) n)

--1(h)
th_prime :: Int -> Int
th_prime n = th_primes 1 n

--1(i)
gedel_size :: Int -> Int
gedel_size n = iff (neq n 0) (_R (const 0) (\i past _ -> iff (multiple n i) (iff (is_prime i) (_Z past) past) past) [] n) (undefined)

--1(j)
gedel_th_elem :: Int -> Int -> Int
gedel_th_elem num n = plog (th_prime n) num

--1(k)
cons :: Int -> Int -> Int
cons k num = _R (const (deg 2 k)) (\i past _ -> mul past (deg (th_prime (add i 2)) (gedel_th_elem num (_Z i)))) [] (gedel_size num)

-- 2
-- 4 = _Z $ _Z $ _Z $ _Z 0
-- `(` - 3  `)` - 5
gedel_check_correct :: Int -> Int
gedel_check_correct num = iff (eq 1 (_R one_arr (\i past _ -> iff (eq past 0) 0 (sub (add past 4) (gedel_th_elem num (_Z i))) ) [] (_Z (gedel_size num)))) 1 0

ackermann :: Int -> Int -> Int
ackermann m n= iff (eq m 0) (_Z n) (iff (eq n 0) (ackermann (sub1 m) (_Z 0)) (ackermann (sub1 m) (ackermann m (sub1 n))))