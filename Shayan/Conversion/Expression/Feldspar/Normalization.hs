module Conversion.Expression.Feldspar.Normalization () where

import Prelude ()
import MyPrelude

import qualified Expression.Feldspar.GADTHigherOrder as FGHO
import qualified Expression.Feldspar.MiniWellScoped  as FMWS

import qualified Type.Feldspar.ADT                   as TFA
import qualified Type.Feldspar.GADT                  as TFG

import Variable.Typed     

import Environment.Typed  
 
import Conversion 
import Conversion.Type.Feldspar ()
import Conversion.Variable      ()
import Conversion.Existential   ()

import Singleton

instance (HasSin TFG.Typ t , t ~ t' , r ~ r') => 
         Cnv (FGHO.Exp r t , ()) (FMWS.Exp r' t') where
  cnv (ee , ()) = let ?r = () in case ee of 
    FGHO.ConI i       -> FMWS.ConI <$@> i 
    FGHO.ConB b       -> FMWS.ConB <$@> b 
    FGHO.Var v        -> case sin :: TFG.Typ t of 
      TFG.Int         -> return (FMWS.AppV v Emp)
      TFG.Bol         -> return (FMWS.AppV v Emp)
      TFG.Arr _ _     -> fail "Normalization Error!"
      TFG.Tpl _ _     -> return (FMWS.AppV v Emp)
      TFG.Ary _       -> return (FMWS.AppV v Emp)
    FGHO.Abs _        -> fail "Normalization Error!"
    FGHO.App _ _      -> do Exs1 v tv <- getVar ee 
                            PrfHasSin <- getPrfHasSinM tv
                            DblExsSin es tys <- getArg ee 
                                                (DblExsSin Emp Emp)
                            EqlOut <- eqlOut (sin :: TFG.Typ t) tv
                            EqlArg <- eqlArg tys tv
                            return (FMWS.AppV v es)
    FGHO.Cnd ec et ef -> FMWS.Cnd <$@> ec <*@> et <*@> ef
    FGHO.Whl ec eb ei -> FMWS.Whl <$@> ec <*@> eb <*@> ei
    FGHO.Tpl ef es    -> case sin :: TFG.Typ t of
      TFG.Tpl tf ts   -> do PrfHasSin <- getPrfHasSinM tf 
                            PrfHasSin <- getPrfHasSinM ts
                            FMWS.Tpl <$@> ef <*@> es
      _               -> fail "Impossible!"
    FGHO.Fst e        -> FMWS.Fst <$@> e
    FGHO.Snd e        -> FMWS.Snd <$@> e
    FGHO.Ary el ef    -> case sin :: TFG.Typ t of
      TFG.Ary ta      -> do PrfHasSin <- getPrfHasSinM ta 
                            FMWS.Ary <$@> el <*@> ef
      _               -> fail "Impossible!"                           
    FGHO.Len ea       -> FMWS.Len <$@> ea 
    FGHO.Ind ea ei    -> FMWS.Ind <$@> ea <*@> ei
    FGHO.Let el eb    -> FMWS.Let <$@> el <*@> eb
 
instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb , r ~ r' , ta ~ ta' ,tb ~ tb') =>
         Cnv (FGHO.Exp r  ta  -> FGHO.Exp r  tb , ()) 
             (FMWS.Exp r' ta' -> FMWS.Exp r' tb')
         where
  cnv (ef , ()) = pure (frmRgt . flip (curry cnv) () . ef . 
                        frmRgt . flip (curry cnv) ())

instance (HasSin TFG.Typ t , t' ~ t , r' ~ r) => 
         Cnv (FMWS.Exp r' t' , ()) (FGHO.Exp r t)  where
  cnv (ee , ()) = let ?r = () in case ee of 
    FMWS.ConI i       -> FGHO.ConI <$@> i 
    FMWS.ConB b       -> FGHO.ConB <$@> b 
    FMWS.AppV v es    -> case (sinTyp v , es) of
      (TFG.Int     , Emp)     -> pure (FGHO.Var v)
      (TFG.Bol     , Emp)     -> pure (FGHO.Var v)
      (TFG.Arr _ _ , Ext _ _) -> do Exs1 e te <- fldApp (FGHO.Var v) es 
                                    Rfl <- eqlSin te (sin :: TFG.Typ t) 
                                    return e      
      (TFG.Tpl _ _ , Emp)     -> pure (FGHO.Var v)
      (TFG.Ary _   , Emp)     -> pure (FGHO.Var v)      
    FMWS.Cnd ec et ef -> FGHO.Cnd <$@> ec <*@> et <*@> ef 
    FMWS.Whl ec eb ei -> FGHO.Whl <$@> ec <*@> eb <*@> ei
    FMWS.Tpl ef es    -> case sin :: TFG.Typ t of 
      TFG.Tpl tf ts    -> do PrfHasSin <- getPrfHasSinM tf 
                             PrfHasSin <- getPrfHasSinM ts
                             FGHO.Tpl <$@> ef <*@> es
      _               -> fail "Impossible!"      
    FMWS.Fst e        -> FGHO.Fst <$@> e 
    FMWS.Snd e        -> FGHO.Snd <$@> e  
    FMWS.Ary el ef    -> case sin :: TFG.Typ t of
      TFG.Ary ta       -> do PrfHasSin <- getPrfHasSinM ta 
                             FGHO.Ary <$@> el <*@> ef
      _               -> fail "Impossible!"                           
    FMWS.Len ea       -> FGHO.Len <$@> ea
    FMWS.Ind ea ei    -> FGHO.Ind <$@> ea <*@> ei
    FMWS.Let el eb    -> FGHO.Let <$@> el <*@> eb
                                        
instance (HasSin TFG.Typ ta , HasSin TFG.Typ tb, ta ~ ta' , tb ~ tb' , r ~ r') =>
         Cnv (FMWS.Exp r  ta  -> FMWS.Exp r  tb , ())  
             (FGHO.Exp r' ta' -> FGHO.Exp r' tb') 
         where
  cnv (ef , ()) = return (frmRgt . flip (curry cnv) () 
                          . ef 
                          . frmRgt . flip (curry cnv) ())
                                     
fldApp :: forall r t ta tb . (t ~ (TFA.Arr ta tb) , HasSin TFG.Typ t) => 
          FGHO.Exp r t ->
          Env (FMWS.Exp r) (ta ': TFG.Arg tb) -> 
          ErrM (Exs1 (FGHO.Exp r) TFG.Typ)   
fldApp e ess = case (sin :: TFG.Typ t , ess) of                              
  (TFG.Arr ta tb@(TFG.Arr _ _) , Ext ea es@(Ext _ _)) -> do
    PrfHasSin <- getPrfHasSinM ta
    PrfHasSin <- getPrfHasSinM tb
    ea' :: FGHO.Exp r ta <- cnv (ea , ())
    fldApp (FGHO.App e ea') es
  (TFG.Arr ta tb              , Ext ea Emp)           -> do 
    PrfHasSin <- getPrfHasSinM ta
    ea' <- cnv (ea , ())
    return (Exs1 (FGHO.App e ea') tb)
  _                                                   ->  
    fail "Impossible!"

getVar :: forall r t. HasSin TFG.Typ t => FGHO.Exp r t ->
          ErrM (Exs1 (Var r) TFG.Typ)
getVar e = case e of
  FGHO.App (FGHO.Var v)       _ -> pure (Exs1 v (sinTyp v))
  FGHO.App ef@(FGHO.App _  _) _ -> getVar ef
  _                             -> fail "Normalization Error!"
 
data DblExsSin :: (ka -> kb -> *) -> ka -> ka -> * where
  DblExsSin :: c2 tf1 t -> c2 tf2 t -> DblExsSin c2 tf1 tf2
    
getArg :: forall r t. FGHO.Exp r t -> DblExsSin Env (FMWS.Exp r) TFG.Typ ->
          ErrM (DblExsSin Env (FMWS.Exp r) TFG.Typ)
getArg e (DblExsSin args tys) = case e of  
  FGHO.App (FGHO.Var _)       ea -> do 
    ea' <- cnv (ea , ())
    return (DblExsSin (Ext ea' args) (Ext (sinTyp ea) tys))
  FGHO.App ef@(FGHO.App _ _) ea -> do 
    ea'  <- cnv (ea , ())
    getArg ef (DblExsSin (Ext ea' args) (Ext (sinTyp ea) tys))
  _                              ->  
    fail "Normalization Error!"
      
data EqlOut :: TFA.Typ -> TFA.Typ -> * where
  EqlOut :: EqlOut (TFG.Out t2) t2

eqlOut :: TFG.Typ t1 -> TFG.Typ t2 -> ErrM (EqlOut t1 t2)
eqlOut TFG.Int         TFG.Int           = return EqlOut
eqlOut TFG.Bol         TFG.Bol           = return EqlOut
eqlOut t               (TFG.Arr _ tb)    = do EqlOut <- eqlOut t tb
                                              return EqlOut
eqlOut (TFG.Ary ta)    (TFG.Ary ta')     = do Rfl <- eqlSin ta ta'
                                              return EqlOut
eqlOut (TFG.Tpl tf ts) (TFG.Tpl tf' ts') = do Rfl <- eqlSin tf tf'
                                              Rfl <- eqlSin ts ts'
                                              return EqlOut
eqlOut _              _                  = fail "Normalization Error!"           

data EqlArg :: [TFA.Typ] -> TFA.Typ -> * where
  EqlArg :: EqlArg (TFG.Arg t2) t2
 
eqlArg :: Env TFG.Typ r -> TFG.Typ t -> ErrM (EqlArg r t)
eqlArg Emp TFG.Int       = return EqlArg
eqlArg Emp TFG.Bol       = return EqlArg
eqlArg (Ext ta ts) (TFG.Arr ta' tb) 
                          = do Rfl    <- eqlSin ta ta'
                               EqlArg <- eqlArg ts tb
                               return EqlArg
eqlArg Emp (TFG.Ary _)   = return EqlArg
eqlArg Emp (TFG.Tpl _ _) = return EqlArg
eqlArg _     _            = fail "Normalization Error!"           
