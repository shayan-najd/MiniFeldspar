{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE RecordWildCards #-}
module UExp  where
import Data.Maybe
import Data.List
import Data.Function
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

type Id = String

data UFun = UFun [(Id, UTyp)] UExp

data UTyp = UTBol | UTDbl

data UExp
    = UDbl Double        -- ^ Double literal
    | UBol Bool          -- ^ Bool literal
    | UVar Id            -- ^ Variable
    | UApp Id [UExp]     -- ^ Function application
    | ULet Id UExp UExp  -- ^ Local binding

instance Show UFun where
    showsPrec p (UFun [] e) = showsPrec p e
    showsPrec p (UFun vts e) = showParen (p>0) (showString "\\ " . foldr (.) (showString "-> ") (map f vts) . showsPrec 0 e)
      where f (v, t) = showParen True (showString v . showString " :: " . showsPrec 0 t) . showString " "

instance Show UTyp where
    showsPrec _ UTDbl = showString "Double"
    showsPrec _ UTBol = showString "Bool"

instance Show UExp where
    showsPrec p (UDbl d) = showsPrec p d
    showsPrec p (UBol b) = showsPrec p b
    showsPrec _ (UVar i) = showString i
    showsPrec p (UApp "if" [c, t, e]) =
      showParen (p>0) (showString "if " . showsPrec 0 c . showString " then " . showsPrec 0 t . showString " else " . showsPrec 0 e)
    showsPrec p (UApp op [a, b]) = showOp p op a b
    showsPrec _ (UApp op _) = error $ "Uxp.show " ++ op
    showsPrec p (ULet i e b) =
      showParen (p>0) (showString "let " . showString i . showString " = " . showsPrec 0 e . showString " in " . showsPrec 0 b)

showOp :: (Show a, Show b) => Int -> String -> a -> b -> String -> String
showOp q sop a b = showParen (q>mp) (showsPrec lp a . showString sop . showsPrec rp b)
  where (lp,mp,rp) = case lookup sop ops of
                    Just (p, AssocLeft)  -> (p,   p, p+1)
                    Just (p, AssocRight) -> (p+1, p, p)
                    Just (p, AssocNone)  -> (p+1, p, p+1)
                    Nothing              -> (9,   9, 10)

ops :: [(String, (Int, Assoc))]
ops = [("+",  (6, AssocLeft)),
       ("-",  (6, AssocLeft)),
       ("*",  (7, AssocLeft)),
       ("/",  (7, AssocLeft)),
       ("==", (4, AssocNone)),
       ("<=", (4, AssocNone)),
       ("&&", (3, AssocRight)),
       ("||", (2, AssocRight))
      ]

parseUFun :: SourceName -> String -> Either ParseError UFun
parseUFun = parse $ do f <- pFun; eof; return f
  where TokenParser{..} = haskell
        pFun = do
            vts <- between (reservedOp "\\")
                           (reservedOp "->")
                           (many $ parens $ do v <- identifier; reservedOp "::"; t <- pTyp; return (v, t))
               <|> return []
            e <- pExp
            return $ UFun vts e
        pTyp = choice [do reserved "Bool"; return UTBol, do reserved "Double"; return UTDbl]
        pExp = choice [pIf, pLet, pOExp]
        pIf = do reserved "if"; c <- pExp; reserved "then"; t <- pExp; reserved "else"; e <- pExp; return $ UApp "if" [c, t, e]
        pLet = do reserved "let"; i <- identifier; reservedOp "="; e <- pExp; reserved "in"; b <- pExp; return $ ULet i e b
        pOExp = buildExpressionParser opTable pAExp
        pAExp = choice [pDbl, pVar, parens pExp]
        pVar = fmap eVar identifier
        pDbl = fmap (either (UDbl . fromInteger) UDbl) naturalOrFloat
        eVar i = if i == "False" then UBol False else if i == "True" then UBol True else UVar i

        opTable = reverse . map (map mkOp) . groupBy ((==) `on` prec) . sortBy (compare `on` prec) $ ops
          where mkOp (s, (_, a)) = Infix (do reservedOp s; return $ \ x y -> UApp s [x, y]) a
                prec = fst . snd

mParseUFun :: String -> Maybe UFun
mParseUFun = either (const Nothing) Just . (parseUFun "")

