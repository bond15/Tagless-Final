

-- 'initial' encoding of object language terms
data Exp = Val Int | Add Exp Exp | Mul Exp Exp

-- 'initial' evaluation of object terms
eval :: Exp -> Int
eval (Val x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

-- 'initial' show of object terms
showe :: Exp -> String
showe (Val x) = show x
showe (Add e1 e2) = (showe e1) ++ " + " ++ (showe e2)
showe (Mul e1 e2) = (showe e1 ) ++ " * " ++ (showe e2)

-- example object language term
t :: Exp
t = (Add (Mul (Val 5) (Val 6)) (Val 7))

-- the Above 'initial' encoding can become extensible
-- with the 'least fixpoint of a functor' encoding
-- and F-Algebras (Data types a la carte)

-- 'final' encoding of object language
class ExpSym repr where
  val :: Int -> repr
  add :: repr -> repr -> repr
  mul :: repr -> repr -> repr

-- 'final' evaluation of object terms
-- an 'interpretation' of the 'object language algebra'
instance ExpSym Int where
  val x = x
  add x y = x + y
  mul x y = x * y

--instance ExpSym String where
 -- val x = show x
 -- add x y = x ++ " + " ++ y
 -- mul x y = x ++ " * " ++ y

-- extensible syntax 
class NegSym repr where
  neg :: repr -> repr

-- extensible semantics (interpretations)
instance NegSym Int where
  neg x = - x

evalf :: Int -> Int
evalf = id

viewf :: String -> String
viewf = id

-- example object language term (with eval semantics)
t2 :: Int
t2 = neg $ add (mul (val 5) (val 6)) (val 7)
