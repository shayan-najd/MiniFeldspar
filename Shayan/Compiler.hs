module Compiler where

import Prelude ()
import MyPrelude hiding (fst , snd)

import Expression.Feldspar.C
import qualified Expression.Feldspar.MiniWellScoped as FMWS

import qualified Type.Feldspar.ADT                  as TFA
import qualified Type.Feldspar.GADT                 as TFG

import qualified Environment.Scoped                 as ES
import qualified Environment.Typed                  as ET

import qualified Variable.Scoped                    as VS

import Conversion
import Conversion.Type.Feldspar ()
import Conversion.Variable ()
import Conversion.Expression.Feldspar.CodeGeneration (pretty) 

import Singleton

type CompileMonad a = StateT (Int,[Var],[Var]) ErrM a

newName :: CompileMonad String
newName = do 
 (i , ps , vs) <- getState  
 put (i+1 , ps , vs) 
 return ("v" ++ (show i))

addParam :: Var -> CompileMonad ()
addParam p = do
  (i , ps , vs) <- getState 
  put (i , p:ps , vs)

addVar :: Var -> CompileMonad ()
addVar v = do
  (i , ps , vs) <- getState 
  put (i , ps , v:vs)

runCompileMonad :: TFA.Typ -> CompileMonad (Exp , [Stmt]) -> Int ->
                   ErrM Func
runCompileMonad ty m i = do ((exp,stmts),(_,ps,vs)) <- runStateT m (i,[],[])
                            pure (Func "func" (ps ++ [("*out",ty)])  
                                  ([Declare (v , t) | (v , t) <- vs] ++ 
                                   stmts ++ [Assign "*out" exp]))

cnvImpLft :: (Cnv (a, r) b, ?r :: r) => 
             a -> CompileMonad b
cnvImpLft  = lift . cnvImp

cmpImp :: (Compilable (t, r), ?r :: r) =>
          t -> CompileMonad (Exp , [Stmt])
cmpImp e = compile (e , ?r)

addInt :: Exp -> Exp -> Exp
addInt el er    = App "addInt" [el , er]

fls :: Exp
fls          = Var "false"

tru :: Exp
tru          = Var "true" 

newTpl :: TFA.Typ -> TFA.Typ -> Exp -> Exp -> Exp
newTpl tf ts ef es = App ("newTpl" ++ show (pretty tf) ++ show (pretty ts))
                     [ef , es]

fst :: TFA.Typ -> TFA.Typ -> Exp -> Exp
fst tf ts e = App ("fstTpl" ++ show (pretty tf) ++ show (pretty ts)) [e]

snd :: TFA.Typ -> TFA.Typ -> Exp -> Exp
snd tf ts e = App ("sndTpl" ++ show (pretty tf) ++ show (pretty ts)) [e]

newAry :: TFA.Typ -> Exp -> Exp
newAry  t e  = App ("newAry" ++ show (pretty t)) [e]

len :: TFA.Typ -> Exp -> Exp
len t e      = App ("lenAry"++ show (pretty t)) [e]

ind :: TFA.Typ -> Exp -> Exp -> Exp
ind t ea ei  = App ("indAry"++ show (pretty t)) [ea , ei] 
 
setAry :: TFA.Typ -> Exp -> Exp -> Exp -> Exp
setAry t ea ei ex = App ("setAry" ++ show (pretty t)) [ea , ei , ex]

ltdInt :: Exp -> Exp -> Exp
ltdInt el er    = App "ltdInt" [el , er]

cmx :: Exp -> Exp -> Exp
cmx er ei    = App "cmx" [er , ei] 

class Compilable t where
 compile :: t -> CompileMonad (Exp , [Stmt])
  
instance (HasSin TFG.Typ t, n ~ Len r) => 
         Compilable (FMWS.Exp r t, ES.Env n String) where
  compile (ee , r) = let ?r = r in do 
    let t = sin :: TFG.Typ t 
    t'  <- cnvImpLft t 
    case ee of
      FMWS.Tmp  x        -> pure (Var x , [])
      FMWS.ConI i        -> pure (Num i , [])
      FMWS.ConB True     -> pure (tru   , [])
      FMWS.ConB False    -> pure (fls   , [])
      FMWS.ConF f        -> pure (Flt f , [])
      FMWS.AppV v ET.Emp -> do v' :: VS.Var n <- cnvImpLft v 
                               ve             <- cnvImpLft v'
                               pure (Var ve , [])
      FMWS.AppV v es     -> do v' :: VS.Var n <- cnvImpLft v 
                               ve             <- cnvImpLft v'
                               (es' , ss)     <- cnvETEnv (sinTypOf v t) es
                               pure (App ve es' ,concat ss)
      FMWS.Cnd ec et ef  -> do v <- newName  
                               addVar (v , t')         
                               (ec' , sc) <- cmpImp ec
                               (et' , st) <- cmpImp et
                               (ef' , sf) <- cmpImp ef
                               return (Var v
                                      , sc ++ 
                                        [If ec' 
                                          (st ++ [Assign v et'])
                                          (sf ++ [Assign v ef'])])
      FMWS.Whl ec eb ei -> do xs <- newName
                              addVar (xs , t')
                              (ec' , sc) <- cmpImp (ec (FMWS.Tmp xs))
                              (eb' , sb) <- cmpImp (eb (FMWS.Tmp xs))
                              (ei' , si) <- cmpImp ei 
                              return ( Var xs
                                     , si ++ 
                                       [Assign xs ei'] ++
                                       sc ++
                                       [Whl ec' 
                                        (sb ++ 
                                         [Assign xs eb'] ++ sc)])
      FMWS.Tpl ef es           -> case TFG.getPrfHasSinTpl t of
       (PrfHasSin , PrfHasSin) -> do let TFA.Tpl tf ts = t'
                                     (ef' , sf) <- cmpImp ef
                                     (es' , ss) <- cmpImp es
                                     return (newTpl tf ts ef' es', sf ++ ss)
      FMWS.Fst e               -> do TFA.Tpl tf ts <- cnvImpLft (sinTypOf e t) 
                                     (e'  , se) <- cmpImp e
                                     return (fst tf ts e' , se)
      FMWS.Snd e               -> do TFA.Tpl tf ts <- cnvImpLft (sinTypOf e t) 
                                     (e' , se) <- cmpImp e
                                     return (snd tf ts e' , se)
      FMWS.Ary l f             -> case TFG.getPrfHasSinAry t of 
        PrfHasSin              -> do let TFA.Ary ta = t'
                                     xl <- newName
                                     addVar (xl , TFA.Int)
                                     xa <- newName
                                     addVar (xa , t')
                                     xi <- newName
                                     addVar (xi , ta)
                                     (el , sl) <- cmpImp l
                                     (ef , sf) <- cmpImp (f (FMWS.Tmp xi))
                                     return ( Var xa
                                            ,   sl ++ 
                                              [ Assign xl el
                                              , Assign xa (newAry ta (Var xl)) 
                                              , Assign xi (Num 0) 
                                              , Whl (ltdInt (Var xi) (Var xl))
                                                (sf ++ 
                                                 [ Assign xa (setAry ta (Var xa) 
                                                               (Var xi) ef)
                                                 , Assign xi (addInt (Var xi) 
                                                              (Num 1))])])
      FMWS.Len e               -> do TFA.Ary ta <- cnvImpLft (sinTypOf e t) 
                                     (e'  , se) <- cmpImp e
                                     return (len ta e' , se) 
      FMWS.Ind ea ei           -> do TFA.Ary ta <- cnvImpLft (sinTypOf ea t) 
                                     (ea' , sa) <- cmpImp ea 
                                     (ei' , si) <- cmpImp ei 
                                     return (ind ta ea' ei' , sa ++ si)
      FMWS.Let el eb           -> do xl <- newName
                                     tl <- cnvImpLft (sinTypOf el t)
                                     addVar (xl , tl)
                                     (el' , sl) <- cmpImp el
                                     (eb' , sb) <- cmpImp (eb (FMWS.Tmp xl))
                                     return (eb' , sl ++ 
                                                   [Assign xl el'] ++ 
                                                   sb)   
      FMWS.Cmx er ei           -> do (er' , sr) <- cmpImp er 
                                     (ei' , si) <- cmpImp ei 
                                     return (cmx er' ei' , sr ++ si)

instance (n ~ Len r , HasSin TFG.Typ t , Compilable (b , ES.Env n String)) => 
         Compilable (FMWS.Exp r t -> b , ES.Env n String) where
 compile (ef , r) = let ?r = r in
   do v  <- newName
      t' <- cnvImpLft (sin :: TFG.Typ t)
      addParam (v , t')
      cmpImp (ef (FMWS.Tmp v))
                                    
cnvETEnv :: ( n ~ Len rr , r ~ TFG.Arg t, HasSin TFG.Typ t 
           , ?r :: ES.Env n String) => 
           TFG.Typ t -> ET.Env (FMWS.Exp rr) r ->
           CompileMonad ([Exp] , [[Stmt]])
cnvETEnv t@(TFG.Arr _  tb) (ET.Ext e ess) = case TFG.getPrfHasSinArr t of
  (PrfHasSin , PrfHasSin) -> do (ee , se) <- cmpImp e 
                                (es , ss) <- cnvETEnv tb ess
                                return (ee : es , se : ss) 
cnvETEnv _                 ET.Emp         = return ([],[])                  
cnvETEnv _                 _              = impossibleM

scompileWith :: Compilable (a , ES.Env n String) => 
                [Var] -> TFG.Typ t -> ES.Env n String -> Int -> a -> ErrM String 
scompileWith vs t r i e = let ?r = r in
                        do t' :: TFA.Typ <- cnvImp t 
                           c <- runCompileMonad t' 
                                (do mapM_ addParam vs
                                    cmpImp e) i
                           return ("#include \"header.h\"\n\n" ++ 
                                   ((show . pretty) c))
 
scompile :: Compilable (a , ES.Env n String) => 
                TFG.Typ t -> ES.Env n String -> a -> ErrM String 
scompile t r e = scompileWith [] t r 0 e  

icompileWith :: Compilable (a , ES.Env n String) => 
                [Var] -> TFG.Typ t -> ES.Env n String -> Int -> a -> IO ()
icompileWith vs t r i e = (putStrLn . frmRgt . scompileWith vs t r i) e
