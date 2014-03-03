module Expression.Feldspar.ADTUntypedDebruijn where
 
import Data.Nat

data Exp = ConI Integer 
         | ConB Bool 
         | Var Nat  
         | Abs Exp
         | App Exp Exp  
         | Cnd Exp Exp Exp 
         | Whl Exp Exp Exp 
         | Tpl Exp Exp
         | Fst Exp 
         | Snd Exp
         | Ary Exp Exp
         | Len Exp 
         | Ind Exp Exp 
         | Let Exp Exp
         deriving Eq
                  
fre :: Exp -> Nat                  
fre ee = case ee of
  ConI _        -> Zro
  ConB _        -> Zro
  Var  n        -> Suc n 
  Abs  eb       -> let Suc n = fre eb in n
  App  ef ea    -> fre ef `max` fre ea
  Cnd  ec et ef -> fre ec `max` fre et `max` fre ef
  Whl  ec eb ei -> fre ec `max` fre eb `max` fre ei
  Tpl  ef es    -> fre ef `max` fre es
  Fst  e        -> fre e
  Snd  e        -> fre e
  Ary  el ef    -> fre el `max` fre ef
  Len  e        -> fre e
  Ind  e ei     -> fre e  `max` fre ei
  Let  el eb    -> fre el `max` let Suc n = fre eb in n