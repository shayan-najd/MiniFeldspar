module Conversion.Expression.Feldspar.CodeGeneration where

import Prelude ()
import MyPrelude hiding ((<>))

import Text.PrettyPrint
import qualified Data.List

import qualified Type.Feldspar.ADT as TFA
import Expression.Feldspar.C

class Pretty a where
 pretty :: a -> Doc

instance Pretty Exp where
 pretty (Var x)     = text x   
 pretty (Flt f)     = text (show f++"f")
 pretty (Num i)     = text (show i++"u")
 pretty (App op es) = text op <+> parens (commaCat (fmap pretty es))
 
instance Pretty Stmt where
 pretty (Whl e s) = text "while" <+> parens (pretty e) 
  $+$ pretty s
 pretty (If e1 e2 e3) = text "if" <+> parens (pretty e1) 
  $+$ pretty e2  
  $+$ text "else" 
  $+$ pretty e3 
 pretty (Grp ss) = lbrace
                   $+$ nest 2 (vcat (fmap pretty ss))
                   $+$ rbrace
 pretty (Assign v e)      = text v <+> text "=" <+> pretty e <> semi
 pretty (Declare (v , t)) = pretty t <+> text v <> semi
 
instance Pretty Func where
 pretty (Func name vs body) = 
  text "void" <+> text name 
  <+> parens (commaCat (fmap pretty vs) )
  $+$ pretty body
  
instance Pretty Var where
 pretty (v,t) = pretty t <+> text v

instance Pretty TFA.Typ where
  pretty t = case t of
    TFA.Int     -> text "Int"
    TFA.Bol     -> text "Bol"
    TFA.Flt     -> text "Flt"
    TFA.Cmx     -> text "Cmx"
    TFA.Tpl a b -> text "Tpl" <> pretty a <> pretty b
    TFA.Ary a   -> text "Ary" <> pretty a
    TFA.Arr _ _ -> impossible

commaCat :: [Doc] -> Doc
commaCat ds = foldl1 (<>) (Data.List.intersperse (comma<>space) ds)