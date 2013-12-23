{-# OPTIONS_GHC -Wall #-}
module ADTExplicitPolymorphic where

import Expression.ADTExplicitPolymorphic
import Variable.ADT
import Environment.ADT
import Value.ADT
import TypeCheckingClass
import ErrorMonad

import qualified Type.ADTSimple as AS

-- Evaluation of expressions under specific environment of values 
evl :: Exp a -> [Val] -> ErrM Val
evl (Con _ i)     _ = return (Num i)
evl (Var _ x)     r = get x r
evl (Abs _ eb)    r = return (Fun (\ va -> case evl eb (va : r) of 
                                      Rgt vb -> vb
                                      Lft  s  -> error s))
evl (App _ ef ea) r = do vf <- evl ef r
                         va <- evl ea r
                         vf `app` va 
evl (Add _ el er) r = do vl <- evl el r 
                         vr <- evl er r      
                         vl `add` vr

-- Typechecking and returning the type, if successful
chk :: Chk m a => Exp a -> [a] -> m a 
chk (Con t _)     _ = do eqlInt t 
                         return t
chk (Var t x)     r = do t' <- get x r
                         eql t t'
                         return t
chk (Abs t eb)    r = do (ta , tb) <- eqlArr t    
                         tb'       <- chk eb (ta : r)
                         eql tb tb'
                         return t
chk (App t ef ea) r = do tf        <- chk ef r
                         ta'       <- chk ea r
                         (ta , tb) <- eqlArr tf
                         eql ta ta' 
                         eql t  tb
                         return t
chk (Add t el er) r = do tl <- chk el r
                         tr <- chk er r
                         eqlInt tl 
                         eqlInt tr 
                         eqlInt t 
                         return t
  
-- An example expression doubling the input number
dbl :: Exp AS.Typ
dbl = Abs (AS.Int `AS.Arr` AS.Int) (Add AS.Int (Var AS.Int Zro) (Var AS.Int Zro))

-- An example expression composing two types
compose :: AS.Typ -> AS.Typ -> AS.Typ -> Exp AS.Typ
compose ta tb tc = Abs (AS.Arr (AS.Arr tb tc) 
                        (AS.Arr (AS.Arr ta tb) 
                         (AS.Arr ta tc))) 
                (Abs (AS.Arr (AS.Arr ta tb) (AS.Arr ta tc))
                 (Abs (AS.Arr ta tc)
                  (App tc (Var (AS.Arr tb tc) (Suc (Suc Zro))) 
                   (App tb (Var (AS.Arr ta tb) (Suc Zro)) (Var ta Zro)))))

-- An example expression representing the Integer 4
four :: Exp AS.Typ
four = App AS.Int 
       (App (AS.Arr AS.Int AS.Int) 
        (App (AS.Arr (AS.Arr AS.Int AS.Int) (AS.Arr AS.Int AS.Int)) 
         (compose AS.Int AS.Int AS.Int)  dbl) dbl) (Con AS.Int 1)
 
-- Two simple test cases
test :: Bool
test = (case evl four [] of 
          Rgt (Num 4) -> True
          _           -> False) 
       && ((chk four [] :: ErrM AS.Typ) == Rgt AS.Int)