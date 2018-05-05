data Exp = X
            | Const Double
            | Neg Exp
	  | Del Exp
            | Add Exp Exp
            | Mul Exp Exp
            deriving (Show, Eq)

calcDer :: Exp -> Exp
calcDer (Mul e1 e2) = Add (Mul (calcDer e1) e2) (Mul e1 (calcDer e2))
calcDer (Add e1 e2) = Add (calcDer e1) (calcDer e2)
calcDer (Del e) = Neg (Mul (calcDer e) (Del (Mul e e)))
calcDer (Neg e) = Neg (calcDer e)
calcDer (Const a) = Const 0
calcDer X = Const 1

eval :: Exp -> Exp
eval (Mul (Const a) (Const b)) = Const (a * b)
eval (Add (Const a) (Const b)) = Const (a + b)
eval (Neg (Const a)) = Const ((-1) * a)
eval (Del (Const a)) = Const (1 / a)
eval (Mul e (Const 0)) = Const 0
eval (Mul (Const 0) e) = Const 0
eval (Add e (Const 0)) = e
eval (Add (Const 0) e) = e
eval (Mul e (Const 1)) = e
eval (Mul (Const 1) e) = e
eval (Const a) = Const a
eval X = X
eval (Neg e) | e == (eval e) = Neg e
		        | otherwise = eval (Neg (eval e))
eval (Del e) | e == (eval e) = Del e
			| otherwise = eval (Del (eval e))
eval (Add e1 e2) | e1 == (eval e1) && e2 == (eval e2) = Add e1 e2
                                      | otherwise = eval (Add (eval e1) (eval e2))
eval (Mul e1 e2) | e1 == (eval e1) && e2 == (eval e2) = Mul e1 e2
                                      | otherwise = eval (Mul (eval e1) (eval e2))
									  
instance Num Exp where
    negate  = Neg
    (+)     = Add
    (*)     = Mul
    fromInteger = Const . fromInteger
    abs     = undefined
    signum  = undefined

n :: Double -> Exp
n = Const

--example
res = (eval .calcDer) (n 1 * X)