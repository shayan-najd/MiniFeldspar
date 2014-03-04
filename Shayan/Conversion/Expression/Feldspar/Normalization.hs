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
 
type SinTyp = HasSin FG.Typ
type SinEnv = HasSin (G.Env FG.Typ)
 

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

instance (t ~ t' , r ~ r' , SinEnv r, SinTyp t) => 
         Cnv (FGHO.Exp r t) (MiWS.Exp r' t') where
  cnv ee = cnv (sin :: FG.Typ t , ee)           

instance (t ~ t' , r ~ r' , SinEnv r) => 
         Cnv (FG.Typ t , FGHO.Exp r t) (MiWS.Exp r' t') where
  cnv (t , ee) = case ee of 
    FGHO.ConI i         -> MiWS.ConI <$> pure i 
    FGHO.ConB b         -> MiWS.ConB <$> pure b 
    FGHO.Var v          -> case t of 
      FG.Arr _ _        -> fail "Normalization Error!"
      FG.Ary _          -> return (MiWS.AppV t v G.Emp)
      FG.Int            -> return (MiWS.AppV t v G.Emp)
      FG.Bol            -> return (MiWS.AppV t v G.Emp)
      FG.Tpl _ _        -> return (MiWS.AppV t v G.Emp)
    FGHO.Abs _          -> fail "Normalization Error!"
    FGHO.App _ _ _      -> do Exs2 v rv tv <- getVar t ee
                              DblExsSin es tys <- getArg ee 
                                                  (DblExsSin G.Emp G.Emp)
                              Rfl <- eqlSin rv (sin :: G.Env FG.Typ r)
                              EqlOut <- eqlOut t tv
                              EqlArg <- eqlArg tys tv
                              return (MiWS.AppV tv v es)
    FGHO.Cnd ec et ef   -> MiWS.Cnd <$> cnv (FG.Bol , ec) 
                           <*> cnv (t , et) <*> cnv (t , ef) 
    FGHO.Whl fc fb ei   -> MiWS.Whl <$> cnv (FG.Arr t FG.Bol , fc) 
                           <*> cnv (FG.Arr t t ,fb) <*> cnv (t , ei)
    FGHO.Tpl ef es      -> case t of
      FG.Tpl tf ts      -> MiWS.Tpl <$> cnv (tf , ef) <*> cnv (ts , es)
      _                 -> fail "Impossible!"
    FGHO.Fst ts e       -> MiWS.Fst <$> pure ts <*> cnv (FG.Tpl t  ts , e)
    FGHO.Snd tf e       -> MiWS.Snd <$> pure tf <*> cnv (FG.Tpl tf t  , e)
    FGHO.Ary el f       -> case t of
      FG.Ary ta         -> MiWS.Ary <$> cnv (FG.Int , el) 
                           <*> cnv (FG.Arr FG.Int ta , f)
      _                 -> fail "Impossible!"                           
    FGHO.Len ta ea      -> MiWS.Len <$> pure ta <*> cnv (FG.Ary ta , ea)
    FGHO.Ind ea ei      -> MiWS.Ind <$> cnv (FG.Ary t , ea) <*> cnv (FG.Int , ei)
    FGHO.Let tl el eb   -> MiWS.Let <$> pure tl <*> cnv (tl , el) 
                           <*> cnv (FG.Arr tl t , eb)
   
instance (SinTyp (A.Arr ta tb) , SinEnv r , r ~ r' , ta ~ ta' , tb ~ tb') =>  
         Cnv (FGHO.Exp r  ta  -> FGHO.Exp r  tb ) 
             (MiWS.Exp r' ta' -> MiWS.Exp r' tb')
         where
           cnv e = cnv (sin :: FG.Typ (A.Arr ta tb) , e)

instance (SinEnv r , r ~ r' , ta ~ ta' , tb ~ tb') =>  
         Cnv (FG.Typ (A.Arr ta tb) , FGHO.Exp r  ta  -> FGHO.Exp r  tb ) 
             (MiWS.Exp r' ta' -> MiWS.Exp r' tb')
         where
           cnv (FG.Arr ta tb , ef) = return (frmRgt . curry cnv tb . ef 
                                             . frmRgt . curry cnv ta)

instance (t' ~ t , r' ~ r , SinEnv r , SinTyp t) => 
         Cnv (MiWS.Exp r' t') (FGHO.Exp r t)  where
  cnv e = cnv (sin :: FG.Typ t , e)

instance (t' ~ t , r' ~ r , SinEnv r) => 
         Cnv (FG.Typ t' , MiWS.Exp r' t') (FGHO.Exp r t)  where
  cnv (t , ee) = case ee of 
    MiWS.ConI i         -> FGHO.ConI <$> pure i 
    MiWS.ConB b         -> FGHO.ConB <$> pure b 
    MiWS.AppV tv v G.Emp -> do Rfl <- eqlSin tv t
                               return (FGHO.Var v)
    MiWS.AppV tv@(FG.Arr _ _) v es@(G.Ext _ _) 
                        -> do Exs2 e re te <- fldApp tv (FGHO.Var v) es 
                              Rfl <- eqlSin te t 
                              Rfl <- eqlSin (sin :: G.Env FG.Typ r) re
                              return e
    MiWS.AppV _  _ _    -> fail "Impossible!"                       
    MiWS.Cnd ec et ef   -> FGHO.Cnd <$> cnv (FG.Bol , ec) 
                                    <*> cnv (t , et) <*> cnv (t , ef) 
    MiWS.Whl ec eb ei   -> FGHO.Whl <$> cnv (FG.Arr t FG.Bol , ec)
                                    <*> cnv (FG.Arr t t      , eb)
                                    <*> cnv (t , ei)
    MiWS.Tpl ef es      -> case t of 
      FG.Tpl tf ts      -> FGHO.Tpl <$> cnv (tf , ef) <*> cnv (ts , es)
      _                 -> fail "Impossible!"      
    MiWS.Fst ts e       -> FGHO.Fst <$> pure ts <*> cnv (FG.Tpl t  ts , e)
    MiWS.Snd tf e       -> FGHO.Snd <$> pure tf <*> cnv (FG.Tpl tf t  , e)
    MiWS.Ary el ef      -> case t of
      FG.Ary ta         -> FGHO.Ary <$> cnv (FG.Int , el) 
                           <*> cnv (FG.Arr FG.Int ta , ef)
      _                 -> fail "Impossible!"                           
    MiWS.Len ta ea      -> FGHO.Len <$> pure ta <*> cnv (FG.Ary ta , ea)
    MiWS.Ind ea ei      -> FGHO.Ind <$> cnv (FG.Ary t , ea) <*> cnv (FG.Int , ei)
    MiWS.Let tl el eb   -> FGHO.Let <$> pure tl <*> cnv (tl , el) 
                           <*> cnv (FG.Arr tl t , eb)
                           
instance (SinTyp (A.Arr ta tb) ,SinEnv r, ta ~ ta' , tb ~ tb' , r ~ r') =>   
         Cnv (MiWS.Exp r  ta  -> MiWS.Exp r  tb)  
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb') 
         where
           cnv e = cnv (sin :: FG.Typ (A.Arr ta tb) ,  e)
           
 
instance (SinEnv r, ta ~ ta' , tb ~ tb' , r ~ r') =>   
         Cnv (FG.Typ (A.Arr ta tb) , MiWS.Exp r  ta  -> MiWS.Exp r  tb)  
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb') 
         where
           cnv (FG.Arr ta tb , ef) = return (frmRgt 
                                             . curry cnv tb . ef 
                                             . frmRgt 
                                             . curry cnv ta)
                                     
fldApp :: forall r t ta tb . (SinEnv r , t ~ (A.Arr ta tb)) =>  
          FG.Typ t -> FGHO.Exp r t -> G.Env (MiWS.Exp r) (ta ': MiWS.Arg tb) -> 
          ErrM (Exs2 FGHO.Exp (G.Env FG.Typ) FG.Typ)  
fldApp (FG.Arr ta tb@(FG.Arr _ _)) e (G.Ext ea es@(G.Ext _ _)) = do
  ea' :: FGHO.Exp r ta <- cnv (ta , ea)
  fldApp tb  (FGHO.App ta e ea') es
fldApp (FG.Arr ta tb)              e (G.Ext ea G.Emp)          = do 
  ea' <- cnv (ta , ea)
  return (Exs2 (FGHO.App ta e ea') (sin :: G.Env FG.Typ r) tb)
fldApp _                           _  _                        = 
  fail "Impossible!"
                                     
getVar :: forall r t. SinEnv r => FG.Typ t -> FGHO.Exp r t -> 
          ErrM (Exs2 V.Var (G.Env FG.Typ) FG.Typ)
getVar tb (FGHO.App ta  (FGHO.App tfa eff _) _ ) = 
  getVar (FG.Arr tfa (FG.Arr ta tb)) eff 
getVar tb (FGHO.App tv (FGHO.Var v) _ )            = 
  return (Exs2 v (sin :: G.Env FG.Typ r) (FG.Arr tv tb))
getVar _  _                                        = 
  fail "Normalization Error!"    

data DblExsSin c2 tf1 tf2 where
  DblExsSin :: c2 tf1 t -> c2 tf2 t -> DblExsSin c2 tf1 tf2
    
getArg :: forall r t. SinEnv r => FGHO.Exp r t -> 
          DblExsSin G.Env (MiWS.Exp r) FG.Typ ->
          ErrM (DblExsSin G.Env (MiWS.Exp r) FG.Typ)
getArg (FGHO.App ta (FGHO.App tfa eff efa) ea) (DblExsSin args tys) = do 
  efa' <- cnv (tfa , efa)
  ea'  <- cnv (ta , ea)
  getArg eff (DblExsSin (G.Ext efa' (G.Ext ea' args)) (G.Ext tfa (G.Ext ta tys)))
getArg (FGHO.App ta (FGHO.Var _) ea)           (DblExsSin args tys) = do 
  ea' <- cnv (ta , ea)
  return (DblExsSin (G.Ext ea' args) (G.Ext ta tys))
getArg _                                       _             = 
  fail "Normalization Error!"
