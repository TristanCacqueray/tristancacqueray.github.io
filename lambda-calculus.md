# Lambda Calculus

> This document is a literate haskell file.

```haskell
{-# LANGUAGE LambdaCase #-}
module MyLambdaCalculus where
import Text.Parsec (char, eof, many1, runParser, satisfy, sepBy1, (<|>))

type Name = String
```

Lambda calculus consists of three terms:

```haskell
data Term =
    Var Name
  | Lam Name Term
  | App Term Term
  deriving (Show)
```

- `Var` (Variable) is a character or string representing a parameter or mathematical/logical value.
- `Lam` (Abstraction) is a function definition.
- `App` (Application) applying a function to an argument.

Instead of writting the term manually, we can parse the standard syntax:

```haskell
parse :: String -> Term
parse s = let Right term = runParser (termParser <* eof) () "lambda" s in term
  where
    termParser = reorderApp <$> (parensParser <|> lamParser <|> varParser) `sepBy1` char ' '
    parensParser = char '(' *> termParser <* char ')'
    lamParser = Lam <$> (char 'λ' *> varParser <* char '.' >>= \(Var name) -> pure name) <*> termParser
    varParser = Var <$> many1 (satisfy (not . flip elem "()λ. "))
    reorderApp (term : terms) = case terms of
      (x : xs) -> foldl App (App term x) xs
      [] -> term
```

## Church encoding

Boolean:

```haskell
true  = parse "λx.λy.x"
false = parse "λx.λy.y"

and'  = parse "λp.λq.p q p"
or'   = parse "λp.λq.p p q"
not'  = parse "λp.λa.λb.p b a"
```

Omega term can't be beta-reduced:

```haskell
omega = parse "(λx.x x) (λx.x x)"
```

Sum types, see [The visitor pattern is essentially the same thing as Church encoding](https://www.haskellforall.com/2021/01/the-visitor-pattern-is-essentially-same.html):

```haskell
circle    = parse "λradi.λc.λr.c radi"
rectangle = parse "λw.λh.λc.λr.r w h"
area = parse "λs.s (λradi.mul pi (square radi)) (λw.λh.mul h w)"

example_circle = App circle (Var "10")
```

## Alpha conversion

Lambda term's variables may need to be renamed to avoid collision.
Multiple strategies exist, see [bound](https://www.schoolofhaskell.com/user/edwardk/bound),
and here is a naive implementation:

```haskell
-- | Convert conflicting variable name (based on code by Renzo Carbonara)
sub :: Name -> Term -> Term -> Term
sub name term = \case
  Var varName -> if name == varName then term else Var varName
  App t1 t2 -> App (convert t1) (convert t2)
  Lam varName body
    | -- varName shadows name
      name == varName ->
      Lam varName body
    | -- varName conflicts
      isFree varName term ->
      let newName = freshName "x" term
          newBody = sub varName (Var newName) body
       in Lam newName (convert newBody)
    | otherwise ->
      Lam varName (convert body)
  where
    convert = sub name term
    isFree n e = n `elem` freeVars e
    freeVars = \case
      Var n -> [n]
      App f x -> freeVars f <> freeVars x
      Lam n b -> filter (/= n) (freeVars b)
    freshName s e
      | isFree s e = freshName ('x' : s) e
      | otherwise = s
```

## Beta reduction

Lambda term's variables can be replaced on application:

```haskell
betaReduce :: Term -> Term
betaReduce = go []
  where
  go env (Var name) = case lookup name env of
    Just term -> term
    Nothing -> (Var name)
  go env (App t1 t2) = let t2' = go env t2 in case go env t1 of
    Lam name body -> go env (sub name t2' body)
    term -> (App term t2')
  go env (Lam name body) = Lam name (go (filter (not . (==) name . fst) env) body)

main = do
  putStrLn "True and !True are equivalent:"
  print $ betaReduce true
  print $ betaReduce (App not' false)
  putStrLn ""
  putStrLn "Area of a circle of radius 10:"
  print $ betaReduce (App area (App circle (Var "10")))
  putStrLn "Area of a rectangle of size 5x10:"
  print $ betaReduce (App area (App (App rectangle (Var "5")) (Var "10")))
```


> To evaluate the file:
> nix-shell -p ghcid -p "haskellPackages.ghcWithPackages (p: [p.markdown-unlit p.parsec])"
> $ ghcid --test=:main --command "ghci -pgmL markdown-unlit" lambda-calculus.lhs
