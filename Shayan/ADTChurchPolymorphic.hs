{-# OPTIONS_GHC -Wall #-}
module ADTChurchPolymorphic where
 
import Expression.ADTChurchPolymorphic  
import Variable.ADT
import Environment.ADT
import Value
import TypeCheckingClass
import ErrorMonad

import qualified Type.ADTSimple as AS

-- Evaluation of expressions under specific environment of values 
evl :: Exp a -> [Val] -> ErrM Val
evl (Con i)     _ = return (Num i)
evl (Var x)     r = get x r
evl (Abs _  eb) r = return (Fun (\ va -> case evl eb (va : r) of 
                                    Rgt vb -> vb
                                    Lft s  -> error s))
evl (App ef ea) r = do vf <- evl ef r
                       va <- evl ea r
                       vf `app` va 
evl (Add el er) r = do vl <- evl el r 
                       vr <- evl er r      
                       vl `add` vr

-- Typechecking and returning the type, if successful
chk :: Chk m a => Exp a -> [a] -> m a 
chk (Con _)     _ = tInt
chk (Var x)     r = get x r
chk (Abs ta eb) r = do tb <- chk eb (ta : r)
                       ta `tArr` tb
chk (App ef ea) r = do tf  <- chk ef r
                       ta' <- chk ea r
                       (ta , tb) <- eqlArr tf 
                       ta `eql` ta' 
                       return tb
chk (Add el er) r = do tl <- chk el r
                       tr <- chk er r
                       eqlInt tl 
                       eqlInt tr 
                       tInt

-- An example expression doubling the input number
dbl :: Exp AS.Typ
dbl = Abs AS.Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: AS.Typ -> AS.Typ -> AS.Typ -> Exp AS.Typ
compose ta tb tc = Abs (AS.Arr tb tc) 
                (Abs (AS.Arr ta tb) 
                 (Abs ta
                  (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))))

-- An example expression representing the Integer 4
four :: Exp AS.Typ
four = (compose AS.Int AS.Int AS.Int `App` dbl `App` dbl) `App` (Con 1)

-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (Num 4) -> True
          _           -> False) 
       && (chk four [] == Rgt AS.Int)