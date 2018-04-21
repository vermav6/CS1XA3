{-|
Module      : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
Copyright   : (c) Varun Verma @2018
License     : WTFPL
Maintainer  : vermav6@mcmaster.ca
Stability   : experimental
Portability : POSIX
TODO write a longer description of the module,
containing some commentary with @some markup@.
-}

-------------Code cited from Siddik1 (Khizar)--------------------

module ExprParser (parseExprD,parseExprF) where
 
import ExprType
import Text.Parsec
import Text.Parsec.String
import ExprPretty
 
---------Parser strings represented as Doubles and Floats----------
 
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err -> error $ show err
                  Right expr -> expr
 
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss o
f                  Left err -> error $ show err
                  Right expr -> expr
 
--------------------------------------------------------------------------------
 
exprD :: Parser (Expr Double)
exprD = exprVar <|> exprConstD <|> exprOpD
 
exprF :: Parser (Expr Float)
exprF = exprVar <|> exprConstF <|> exprOpF
 
--------------------------------------------------------------------------------
 
exprConstD :: Parser (Expr Double)
exprConstD = do {
               symbol "Const";
               ss <- double;
               return (Const ss);
             }
 
exprOpD :: Parser (Expr Double)
exprOpD = do {
                s <- symbol "Add" <|> symbol "Mult";
                ss <- between (symbol "(") (symbol ")") (exprVar <|> exprConstD <|> exprOpD <|> exprOpD);
                ss' <- between (symbol "(") (symbol ")") (exprVar <|> exprConstD <|> exprOpD <|> exprOpD);
 
                if s == "Add" then
                  return (Add ss ss');
                else
                  return (Mult ss ss')
              }
 
--------------------------------------------------------------------------------
 
exprConstF :: Parser (Expr Float)
exprConstF = do {
               symbol "Const";
               ss <- float;
               return (Const ss);
             }
 
exprOpF :: Parser (Expr Float)
exprOpF = do {
                s <- symbol "Add" <|> symbol "Mult";
                ss <- between (symbol "(") (symbol ")") (exprVar <|> exprConstF <|> exprOpF);
                ss' <- between (symbol "(") (symbol ")") (exprVar <|> exprConstF <|> exprOpF);
 
                if s == "Add" then
                  return (Add ss ss');
                else
                  return (Mult ss ss')
              }
 
--------------------------------------------------------------------------------
 
exprVar :: Parser (Expr a)
exprVar = do {
               symbol "Var";
               ss <- many1 letter;
               return (Var ss);
             }
 
parens :: Parser a -> Parser a
parens p = do { char '(';
               cs <- p;
               char ')';
               return cs }
 
symbol :: String -> Parser String
symbol ss = let
 symbol' :: Parser String
 symbol' = do { spaces;
                ss' <- string ss;
                spaces;
                return ss' }
 in try symbol'
 
removeRight (Right ss) = ss

{- Functions for all types -}

 
digits :: Parser String
digits = many1 digit
 
-- for negative numbers
negDigits :: Parser String
negDigits = do { neg <- symbol "-";
                dig <- digits;
                return (neg ++ dig) }
 
integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits
 
decimalDigits :: Parser String
decimalDigits = do { d <- char '.';
                     rm <- digits;
                     return $ d:rm }
 
decimalDigits' :: Parser String
decimalDigits' = do { ds <- try negDigits <|> digits;
                   rs <- try decimalDigits <|> return "";
                   return $ ds ++ rs }
 
double :: Parser Double
double = fmap read $ decimalDigits'
 
float :: Parser Float
float = fmap read $ decimalDigits'