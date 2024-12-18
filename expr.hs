data Expr = Lint Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr

eval :: Expr -> Int
eval (Lint n) = n
eval (Add expA expB) = eval expA + eval expB
eval (Sub expA expB) = eval expA - eval expB
eval (Mul expA expB) = eval expA * eval expB


expression = Add (Mul (Lint 2) (Lint 5)) (Mul (Add (Lint 2) (Lint 3)) (Lint 1))