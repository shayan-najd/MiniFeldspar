module Expression.Feldspar.ADTUntypedDebruijn where

import MyPrelude

import Variable.Plain

data Exp = ConI Integer
         | ConB Bool
         | ConF Float
         | Var  Var
         | Abs  Exp
         | App  Exp Exp
         | Cnd  Exp Exp Exp
         | Whl  Exp Exp Exp
         | Tpl  Exp Exp
         | Fst  Exp
         | Snd  Exp
         | Ary  Exp Exp
         | Len  Exp
         | Ind  Exp Exp
         | Let  Exp Exp
         | Cmx  Exp Exp

deriving instance Eq   Exp
deriving instance Show Exp

fre :: Exp -> [Nat]
fre ee = case ee of
  ConI _        -> []
  ConB _        -> []
  ConF _        -> []
  Var  n        -> [n]
  Abs  eb       -> drpZro (fre eb)
  App  ef ea    -> fre ef ++ fre ea
  Cnd  ec et ef -> fre ec ++ fre et ++ fre ef
  Whl  ec eb ei -> drpZro (fre ec) ++ drpZro (fre eb) ++ fre ei
  Tpl  ef es    -> fre ef ++ fre es
  Fst  e        -> fre e
  Snd  e        -> fre e
  Ary  el ef    -> fre el ++ drpZro (fre ef)
  Len  e        -> fre e
  Ind  e ei     -> fre e  ++ fre ei
  Let  el eb    -> fre el ++ drpZro (fre eb)
  Cmx  er ei    -> fre er ++ fre ei
  where
    drpZro = fmap prd . filter (/= Zro)