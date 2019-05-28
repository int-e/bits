{-# LANGUAGE FlexibleContexts #-}

-- a macro preprocessor for bit** programming
--
-- supports arithmetic, conditionals, and for loops
--
-- syntax:
-- file    ::= { comment | code | macro }*
-- comment ::= '?' { non-'?' }* '?'
-- code    ::= ',' body ','
-- macro   ::= name [ '(' name { ',' name }* ')' ] '=' body ';'
-- body    ::= { slice }*
-- slice   ::= name [ '(' body { ',' body }* ')' ]
--           | '{' expr '}*'
--           | 'if' '(' expr ',' body [ ',' body ] ')'
--           | 'for' '(' name '=' expr ',' expr ',' body ')'
--           | { non-ws, non-alpha character not in "=(){}?" }*
--           | comment
-- expr    ::= un expr | expr bin expr | natural | name | '(' expr ')'
-- un      ::= '-' | '+' | '~'
-- bin     ::= '*" | '/' | '<<' | '>>' | '<=' | '<' | '>=' | '>'
--           | '&' | '^' | '|'
-- name    ::= { alpha character | '_' }+
-- nat     ::= { digit }+

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Bits
import Data.Char
import qualified Data.Map as M

type Macro = ([String], Body)
type Env = M.Map String Macro
type Body = [Slice]
data Slice = Verb String | Macro String [Body] | Expr Expr |
    If Expr Body Body | For String Expr Expr Body
data Expr = Binary (Integer -> Integer -> Integer) Expr Expr |
    Unary (Integer -> Integer) Expr | Nat Integer | Var String

expr :: Parsec String u Expr
expr = buildExpressionParser table term <?> "expression"

term = tok "(" *> expr <* tok ")" <|> Nat <$> natural <|> Var <$> name <?> "simple expression"

table = [
    [prefix "-" negate, prefix "+" id, prefix "~" complement],
    [binary "*" (*) AssocLeft, binary "/" div AssocLeft],
    [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft],
    [binary "<<" shl AssocLeft, binary ">>" shr AssocLeft],
    [binary "<=" (cmp (<=)) AssocLeft, binary "<" (cmp (<)) AssocLeft,
     binary ">=" (cmp (>=)) AssocLeft, binary ">" (cmp (>)) AssocLeft],
    [binary "&" (.&.) AssocLeft],
    [binary "^" xor AssocLeft],
    [binary "|" (.|.) AssocLeft]
    ]
  where
    shl n s | s >= 2^16 = error $ "<< amount too large: " ++ show s
            | otherwise = n `shiftL` fromInteger s
    shr n s | s >= 2^16 = error $ ">> amount too large: " ++ show s
            | otherwise = n `shiftR` fromInteger s
    cmp o n m = if o n m then 1 else 0

binary  name fun = Infix   (tok name >> return (Binary fun)) {- assoc -}
prefix  name fun = Prefix  (tok name >> return (Unary fun))
postfix name fun = Postfix (tok name >> return (Unary fun))

natural :: Parsec String u Integer
natural = read <$> many1 (oneOf ['0'..'9']) <* spaces

name :: Parsec String u String
name = try $ many1 (satisfy isAlpha') <* spaces

body :: Parsec String u Body
body = many slice
slice = if' <|> for' <|> verb <|> macro <|> expr' <|> comment *> return (Verb [])
  where
    if' = do
        tok "if" *> tok "("
        If <$> expr <* tok "," <*> body
           <*> (tok "," *> body <|> return []) <* tok ")"
    for' = do
        tok "for" *> tok "("
        For <$> name <* tok "=" <*> expr <* tok "," <*> expr <* tok ","
            <*> body <* tok ")"
    verb = Verb <$> many1 (satisfy (\c -> isPrint c && not (c `elem` "=(){},?")
        && not (isAlpha' c) && not (isSpace c))) <* spaces
    macro = Macro <$> name <*> (tok "(" *> sepBy1 body (tok ",") <* tok ")" <|>
        return [])
    expr' = Expr <$> (tok "{" *> expr <* tok "}")

evalExpr :: Env -> Expr -> Integer
evalExpr env = go
  where
    go (Binary o e1 e2) = o (go e1) (go e2)
    go (Unary o e) = o (go e)
    go (Nat n) = n
    go (Var s) = read (evalSlice env (Macro s [])) -- error?

evalBody :: Env -> Body -> String
evalBody env = (>>= evalSlice env)

evalSlice :: Env -> Slice -> String
evalSlice env = go
  where
    go (Verb s) = s
    go (Macro s bs) = do
        let (vs, b) = env M.! s
            xs = map (evalBody env) bs
            env' = foldl (\m (v, x) -> M.insert v ([], [Verb x]) m) env
                (zip vs xs)
        evalBody env' b
    go (Expr e) = show (evalExpr env e)
    go (If c t e) = if evalExpr env c /= 0 then evalBody env t
        else evalBody env e
    go (For v s e b) = do
        i <- [evalExpr env s..evalExpr env e]
        evalBody (M.insert v ([], [Verb (show i)]) env) b

file :: Parsec String Env String
file = spaces *> (concat <$> many (def <|> code <|> comment *> return [])) <* eof
  where
    def = do
        n <- name
        vs <- tok "(" *> sepBy1 name (tok ",") <* tok ")" <|> return []
        tok "="
        b <- body
        tok ","
        modifyState (M.insert n (vs, b))
        return []
    code = evalBody <$> getState <*> (tok "," *> body <* tok ",")

comment = tok "?" *> many (satisfy (/= '?')) *> tok "?"

tok s = try (string s) <* spaces
isAlpha' c = isAlpha c || c == '_'

main = interact main'
main' input = case runParser file M.empty "<stdin>" input of
    Left e -> error (show e)
    Right s -> s
