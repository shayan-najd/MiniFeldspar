module Conversion.Expression.Feldspar.Normalization where

import Prelude hiding (sin)

import qualified Expression.Feldspar.MiniWellScoped  as MiWS
import qualified Expression.Feldspar.GADTHigherOrder as FGHO 

import qualified Variable               as V
import qualified Type.Feldspar          as A
import qualified Singleton.TypeFeldspar as FG
import qualified Singleton.Environment  as G

import Conversion 
import Conversion.Type.Feldspar      ()
import Conversion.Variable           ()
import Conversion.Existential        ()
import Existential
import Singleton
 
data EqlOut t1 t2 where
  EqlOut :: EqlOut (MiWS.Out t2) t2

eqlOut :: FG.Typ t1 -> FG.Typ t2 -> ErrM (EqlOut t1 t2)
eqlOut FG.Int         FG.Int           = return EqlOut
eqlOut FG.Bol         FG.Bol           = return EqlOut
eqlOut t              (FG.Arr _ tb)    = do EqlOut <- eqlOut t tb
                                            return EqlOut
eqlOut (FG.Ary ta)    (FG.Ary ta')     = do Rfl <- eqlSin ta ta'
                                            return EqlOut
eqlOut (FG.Tpl tf ts) (FG.Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                            Rfl <- eqlSin ts ts'
                                            return EqlOut
eqlOut _              _                = fail "Normalization Error!"           

data EqlArg t1 t2 where
  EqlArg :: EqlArg (MiWS.Arg t2) t2
 
eqlArg :: G.Env FG.Typ r -> FG.Typ t -> ErrM (EqlArg r t)
eqlArg G.Emp FG.Int           = return EqlArg
eqlArg G.Emp FG.Bol           = return EqlArg
eqlArg (G.Ext ta ts) (FG.Arr ta' tb) 
                              = do Rfl    <- eqlSin ta ta'
                                   EqlArg <- eqlArg ts tb
                                   return EqlArg
eqlArg G.Emp (FG.Ary _)       = return EqlArg
eqlArg G.Emp (FG.Tpl _ _)     = return EqlArg
eqlArg _              _       = fail "Normalization Error!"           
 
instance (t ~ t' , r ~ r') => 
         Cnv (FGHO.Exp r t , G.Env FG.Typ r , FG.Typ t) (MiWS.Exp r' t') where
  cnv (ee , r , t) = case ee of 
    FGHO.ConI i         -> MiWS.ConI <$> pure i 
    FGHO.ConB b         -> MiWS.ConB <$> pure b 
    FGHO.Var v          -> case t of 
      FG.Arr _ _        -> fail "Normalization Error!"
      FG.Ary _          -> return (MiWS.AppV t v G.Emp)
      FG.Int            -> return (MiWS.AppV t v G.Emp)
      FG.Bol            -> return (MiWS.AppV t v G.Emp)
      FG.Tpl _ _        -> return (MiWS.AppV t v G.Emp)
    FGHO.Abs _          -> fail "Normalization Error!"
    FGHO.App _ _ _      -> do Exs2 v rv tv <- getVar ee r t 
                              DblExsSin es tys <- getArg ee r 
                                                  (DblExsSin G.Emp G.Emp)
                              Rfl    <- eqlSin rv r
                              EqlOut <- eqlOut t tv
                              EqlArg <- eqlArg tys tv
                              return (MiWS.AppV tv v es)
    FGHO.Cnd ec et ef   -> MiWS.Cnd <$> cnv (ec , r , FG.Bol) 
                           <*> cnv (et , r , t) <*> cnv (ef , r , t) 
    FGHO.Whl fc fb ei   -> MiWS.Whl <$> cnv (fc , r , FG.Arr t FG.Bol) 
                           <*> cnv (fb , r , FG.Arr t t) <*> cnv (ei , r , t)
    FGHO.Tpl ef es      -> case t of
      FG.Tpl tf ts      -> MiWS.Tpl <$> cnv (ef , r , tf) <*> cnv (es , r , ts)
      _                 -> fail "Impossible!"
    FGHO.Fst ts e       -> MiWS.Fst <$> pure ts <*> cnv (e , r , FG.Tpl t  ts)
    FGHO.Snd tf e       -> MiWS.Snd <$> pure tf <*> cnv (e , r , FG.Tpl tf t)
    FGHO.Ary el f       -> case t of
      FG.Ary ta         -> MiWS.Ary <$> cnv (el , r , FG.Int) 
                           <*> cnv (f , r , FG.Arr FG.Int ta)
      _                 -> fail "Impossible!"                           
    FGHO.Len ta ea      -> MiWS.Len <$> pure ta <*> cnv (ea , r , FG.Ary ta)
    FGHO.Ind ea ei      -> MiWS.Ind <$> cnv (ea , r , FG.Ary t) 
                           <*> cnv (ei , r , FG.Int)
    FGHO.Let tl el eb   -> MiWS.Let <$> pure tl <*> cnv (el , r , tl) 
                           <*> cnv (eb , r , FG.Arr tl t)
 
instance (r ~ r' , ta ~ ta' , tb ~ tb') =>  
         Cnv (FGHO.Exp r  ta  -> FGHO.Exp r  tb 
             , G.Env FG.Typ r , FG.Typ (A.Arr ta tb)) 
             (MiWS.Exp r' ta' -> MiWS.Exp r' tb')
         where
           cnv ( ef , r , FG.Arr ta tb) = return (frmRgt 
                                                 . (\e -> cnv (e , r , tb)) 
                                                 . ef 
                                                 . frmRgt 
                                                 . (\e -> cnv (e , r , ta)))

instance (t' ~ t , r' ~ r) => 
         Cnv (MiWS.Exp r' t' , G.Env FG.Typ r , FG.Typ t') (FGHO.Exp r t)  where
  cnv (ee , r , t) = case ee of 
    MiWS.ConI i         -> FGHO.ConI <$> pure i 
    MiWS.ConB b         -> FGHO.ConB <$> pure b 
    MiWS.AppV tv v G.Emp -> do Rfl <- eqlSin tv t
                               return (FGHO.Var v)
    MiWS.AppV tv@(FG.Arr _ _) v es@(G.Ext _ _) 
                        -> do Exs2 e re te <- fldApp (FGHO.Var v) r tv es 
                              Rfl <- eqlSin te t 
                              Rfl <- eqlSin r re
                              return e
    MiWS.AppV _  _ _    -> fail "Impossible!"                       
    MiWS.Cnd ec et ef   -> FGHO.Cnd <$> cnv (ec , r , FG.Bol) 
                                    <*> cnv (et , r , t) <*> cnv (ef , r , t) 
    MiWS.Whl ec eb ei   -> FGHO.Whl <$> cnv (ec , r , FG.Arr t FG.Bol)
                                    <*> cnv (eb , r , FG.Arr t t)
                                    <*> cnv (ei , r , t)
    MiWS.Tpl ef es      -> case t of 
      FG.Tpl tf ts      -> FGHO.Tpl <$> cnv (ef , r , tf) 
                                    <*> cnv (es , r , ts)
      _                 -> fail "Impossible!"      
    MiWS.Fst ts e       -> FGHO.Fst <$> pure ts 
                           <*> cnv (e , r , FG.Tpl t  ts)
    MiWS.Snd tf e       -> FGHO.Snd <$> pure tf 
                           <*> cnv (e , r , FG.Tpl tf t)
    MiWS.Ary el ef      -> case t of
      FG.Ary ta         -> FGHO.Ary <$> cnv (el , r , FG.Int) 
                           <*> cnv (ef , r , FG.Arr FG.Int ta)
      _                 -> fail "Impossible!"                           
    MiWS.Len ta ea      -> FGHO.Len <$> pure ta <*> cnv (ea , r , FG.Ary ta)
    MiWS.Ind ea ei      -> FGHO.Ind <$> cnv (ea , r , FG.Ary t) 
                           <*> cnv (ei , r , FG.Int)
    MiWS.Let tl el eb   -> FGHO.Let <$> pure tl <*> cnv (el , r , tl) 
                           <*> cnv (eb , r , FG.Arr tl t)
                                        
instance (ta ~ ta' , tb ~ tb' , r ~ r') =>   
         Cnv (MiWS.Exp r  ta  -> MiWS.Exp r  tb
             , G.Env FG.Typ r , FG.Typ (A.Arr ta tb))  
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb') 
         where
           cnv (ef , r , FG.Arr ta tb) = return (frmRgt 
                                             . (\ e -> cnv (e , r , tb)) 
                                             . ef 
                                             . frmRgt 
                                             . (\ e -> cnv (e , r , ta)))
                                     
fldApp :: forall r t ta tb . t ~ (A.Arr ta tb) => 
          FGHO.Exp r t -> G.Env FG.Typ r -> FG.Typ t -> 
          G.Env (MiWS.Exp r) (ta ': MiWS.Arg tb) -> 
          ErrM (Exs2 FGHO.Exp (G.Env FG.Typ) FG.Typ)  
fldApp e r (FG.Arr ta tb@(FG.Arr _ _)) (G.Ext ea es@(G.Ext _ _)) = do
  ea' :: FGHO.Exp r ta <- cnv (ea , r , ta)
  fldApp (FGHO.App ta e ea') r tb es
fldApp e r (FG.Arr ta tb)              (G.Ext ea G.Emp)          = do 
  ea' <- cnv (ea , r , ta)
  return (Exs2 (FGHO.App ta e ea') r tb)
fldApp _ _                           _  _                        = 
  fail "Impossible!"
                                     
getVar :: forall r t. FGHO.Exp r t -> G.Env FG.Typ r -> FG.Typ t ->  
          ErrM (Exs2 V.Var (G.Env FG.Typ) FG.Typ)
getVar (FGHO.App ta  (FGHO.App tfa eff _) _ ) r tb = 
  getVar eff r (FG.Arr tfa (FG.Arr ta tb))
getVar (FGHO.App tv (FGHO.Var v) _ )          r tb = 
  return (Exs2 v r (FG.Arr tv tb))
getVar _ _  _                                      = 
  fail "Normalization Error!"    

data DblExsSin c2 tf1 tf2 where
  DblExsSin :: c2 tf1 t -> c2 tf2 t -> DblExsSin c2 tf1 tf2
    
getArg :: forall r t. FGHO.Exp r t -> G.Env FG.Typ r ->
          DblExsSin G.Env (MiWS.Exp r) FG.Typ ->
          ErrM (DblExsSin G.Env (MiWS.Exp r) FG.Typ)
getArg (FGHO.App ta (FGHO.App tfa eff efa) ea) r (DblExsSin args tys) = do 
  efa' <- cnv (efa , r , tfa)
  ea'  <- cnv (ea  , r , ta)
  getArg eff r (DblExsSin (G.Ext efa' (G.Ext ea' args))(G.Ext tfa(G.Ext ta tys)))
getArg (FGHO.App ta (FGHO.Var _) ea) r           (DblExsSin args tys) = do 
  ea' <- cnv (ea , r , ta)
  return (DblExsSin (G.Ext ea' args) (G.Ext ta tys))
getArg _ _                                       _             = 
  fail "Normalization Error!"
