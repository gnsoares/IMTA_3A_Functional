
-- NOM : 
-- Prenom : 

-- single ligne comment 

-- haskell + dependant types = agda 

-- natural 

data Nat : Set where 
  Z : Nat 
  S : Nat -> Nat 

add : Nat -> Nat -> Nat 
add Z n2 = n2
add (S n1) n2 = S (add n1 n2) 

-- INCREMENTAL PROGRAMMING:
-- C-L = load 
-- ? = dig a hole 
-- C-, = print a hole
-- C-C = break a variable
-- C-Space = fill a hole 
-- C-F = forward hole
-- C-B = backward hole
-- C-R = refine hole 
-- C-N = normalize

-- equal type

data Equal : Nat -> Nat -> Set where 
  case0 : Equal Z Z 
  case1 : {n1 n2 : Nat} -> Equal n1 n2 -> Equal (S n1) (S n2)

-- properties of equal 

equal-refl : (n : Nat) -> Equal n n 
equal-refl Z = case0
equal-refl (S n) = case1 (equal-refl n)

equal-sym : {n1 n2 : Nat} -> Equal n1 n2 -> Equal n2 n1
equal-sym case0 = case0
equal-sym (case1 p12) = case1 (equal-sym p12) 

equal-trans : {n1 n2 n3 : Nat} -> Equal n1 n2 -> Equal n2 n3 -> Equal n1 n3
equal-trans case0 case0 = case0
equal-trans (case1 p12) (case1 p23) = case1 (equal-trans p12 p23) 

-- properties of add

add-right : (n : Nat) -> Equal (add n Z) n 
add-right Z = case0
add-right (S n) = case1 (add-right n)

add-assoc : (n1 n2 n3 : Nat) -> Equal (add (add n1 n2) n3) (add n1 (add n2 n3))
add-assoc Z n2 n3 = equal-refl (add n2 n3)
add-assoc (S n1) n2 n3 = case1 (add-assoc n1 n2 n3)

add-S : (n1 n2 : Nat) -> Equal (add n1 (S n2)) (S (add n1 n2))
add-S Z n2 = equal-refl (S n2)
add-S (S n1) n2 = case1 (add-S n1 n2)

add-commut : (n1 n2 : Nat) -> Equal (add n1 n2) (add n2 n1)
add-commut Z n2 = equal-sym (add-right n2)
add-commut (S n1) n2 = equal-sym (equal-trans (add-S n2 n1) (case1 (add-commut n2 n1)))

-- TP8 : EVAL 

addA : Nat -> Nat -> Nat 
addA Z n2 = n2
addA (S n1) n2 = S (addA n2 n1) 

-- prove the following properties 

addA-right : (n : Nat) -> Equal (addA n Z) n
addA-right n = {!   !} 

addA-commut : (n1 n2 : Nat) -> Equal (addA n1 n2) (addA n2 n1)
addA-commut n1 n2 = {!   !} 

add-addA : (n1 n2 : Nat) -> Equal (add n1 n2) (addA n1 n2)
add-addA n1 n2 = {!   !}
