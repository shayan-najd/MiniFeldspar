{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Existential where

import Expression.Existential
import qualified  Environment.GADT as G
import Value.Existential
import EqualityProof
import ErrorMonad
import qualified GADT  as G

type Env = [Val]

env :: G.Env e -> [Val] -> ErrM e
env G.Emp          []                = return ()
env (t `G.Ext` ts) ((Val x tv) : vs) = do ts' <- env ts vs
                                          Rfl <- eqlTyp t tv 
                                          return (x,ts')
env _              _                 = fail "Scope Error!"  
 
evl :: Exp -> [Val] -> ErrM Val                              
evl (Exp e r t) vs = do r' <- env r vs
                        let v = G.evl e r'
                        return (Val v t)