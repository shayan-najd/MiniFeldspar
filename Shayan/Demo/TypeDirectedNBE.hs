-- Inspired by Dybjer and Flinisky's 
module Demo.TypeDirectedNBE where

import MyPrelude

import Environment.Map

data Exp = Var String | Abs String Exp | App Exp Exp

deriving instance Show Exp
deriving instance Eq Exp

data Typ = Base String | Arrow Typ Typ

type Unq a = State Int a

newVar :: Unq String
newVar = do i <- getState
            put (i + 1)
            return ("x" ++ show i)    
  
data Sem = TM (Unq Exp) | Fun (Monad m => Sem -> m Sem)

eval :: Monad m => Exp -> Env String Sem -> m Sem
eval (Var x)     r = get x r
eval (Abs x t)   r = return (Fun (\ v -> eval t ((x , v) : r)))
eval (App e1 e2) r = do Fun f <- eval e1 r
                        a     <- eval e2 r  
                        f a

reify :: Typ -> Sem -> Unq Exp
reify (Base _)      (TM e)  = e 
reify (Arrow t1 t2) (Fun f) = do v <- newVar
                                 Abs v <$> (do e' <- f (reflect t1 
                                                        (return (Var v)))
                                               reify t2 e')
reify _             _       = fail "Bad Type!"

reflect :: Typ -> Unq Exp -> Sem
reflect (Base _)      e = TM e
reflect (Arrow t1 t2) e = Fun (\ a -> return 
                                      (reflect t2 (App <$> e <*> reify t1 a)))

norm :: Typ -> Exp -> Exp
norm t e = evalState (do r <- eval e []
                         reify t r) 0 

test :: Bool
test = norm (Arrow (Arrow (Base "a") (Base "b")) (Arrow (Base "a") (Base "b")))
       (Abs "f" 
        (Abs "x" 
         (App (Abs "y" (App (Var "f") (Var "y"))) 
          (App (Var "f") (Var "x"))))) == 
       (Abs "x0" (Abs "x1" (App (Var "x0") (App (Var "x0") (Var "x1")))))