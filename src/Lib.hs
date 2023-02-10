{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Lib(parseYaml) where

import Common as C
    ( charP,
      expectIndP,
      expectP,
      fencedP,
      indP,
      nlOptP,
      nonConsumingP,
      nonEmpty,
      optP,
      readP,
      stringP,
      takeWhileP,
      wsOptP )
import Parser as P
import Document as D

import Control.Applicative ((<|>), Alternative (many))
import Data.Char (isAlphaNum)

parseYaml :: String -> Either String Document
parseYaml str = fst <$> runParser yamlP (pipeline str)
  where
    pipeline = unlines . removeEmptyLines . removeTrailingWhiteSpace . removeComments . lines
    
    removeTrailingWhiteSpace =
      map (reverse . dropWhile (== ' ') . reverse)

    removeComments =
      map (takeWhile (/= '#'))

    removeEmptyLines =
      filter (not . null)

yamlP :: Parser Document
yamlP = listP 0 <|> mapP <|> simpleP

scalarP :: Parser Document
scalarP = 
      (D.DString <$> (wsOptP *> stringLitP <* wsOptP))
  <|> stringUnfencedToDocP

stringUnfencedP :: Parser String
stringUnfencedP = trim <$> nonEmpty (takeWhileP (isAlphaNum ||| flip elem "~ .-_"))

stringUnfencedToDocP :: Parser Document
stringUnfencedToDocP = do
  trimmed <- stringUnfencedP

  (D.DInt <$> (wsOptP *> C.readP trimmed <* wsOptP))
    <|> (D.DFloat  <$> (wsOptP *> C.readP trimmed <* wsOptP))
    <|> (wsOptP *> (DNull <$ ifP ((== "null") ||| (== "~")) trimmed) <* wsOptP)
    <|> pure (D.DString trimmed)

ifP :: (a -> Bool) -> a -> Parser a
ifP p a = Parser $ \input -> do
  if p a 
    then return (a, input)
    else Left "Condition unmet"

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p1 ||| p2 = \bool -> p1 bool || p2 bool    

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

stringLitP :: Parser String
stringLitP = C.fencedP '\'' <|> C.fencedP '"'

simpleP :: Parser Document
simpleP = listInBracketsP <|> mapInBracesP <|> kvpAlone <|> scalarP

listInBracketsP :: Parser Document
listInBracketsP = DList <$> (one <|> more)
  where
    one = 
      charP '[' *> wsOptP *>
      optP simpleP 
      <* wsOptP <* charP ']'
    more = 
      charP '[' *> wsOptP *> 
      ((:) <$> simpleP) <*> many (wsOptP *> charP ',' *> wsOptP *> simpleP)
      <* charP ']'

kvpAlone :: Parser Document
kvpAlone = (\kvp -> DMap [kvp]) <$> kvpInBracesP

kvpInBracesP :: Parser (String, Document)
kvpInBracesP = (,) <$> (stringUnfencedP <|> stringLitP) <* wsOptP <* stringP ": " <* wsOptP <*> simpleP

mapInBracesP :: Parser Document
mapInBracesP = DMap <$> (more <|> one)
  where
    one = 
      charP '{' *> wsOptP *> 
      optP kvpInBracesP
      <* wsOptP <* charP '}'
    more =
      charP '{' *> wsOptP *>
      ((:) <$> kvpInBracesP) <*> many (wsOptP *> charP ',' *> wsOptP *> kvpInBracesP)
      <* wsOptP <* charP '}'

listP :: Int -> Parser Document
listP indent = do
  ind <- indP

  first  <- 
      (
        ((() <$ stringP "- ") *> (listP (ind + indent + dashLen) <|> mapP' (ind + indent + dashLen) <|> simpleP))
        <|> (() <$ (dashNl *> nonConsumingP (expectP indP (> (ind + indent))))) *> (listP 0 <|> mapP <|> simpleP)
      ) <* nlOptP
      
  latter <- many
    (
      (
        expectIndP (ind + indent) *> 
        ((dashWs *> (listP (ind + indent + dashLen) <|> mapP' (ind + indent + dashLen) <|> simpleP))
        <|> (() <$ (dashNl *> nonConsumingP (expectP indP (> (ind + indent))))) *> (listP 0 <|> mapP <|> simpleP))
      )
        <* nlOptP
    )

  return $ DList (first:latter)
    where
      dashLen = 2
      mapP' :: Int -> Parser Document
      mapP' ind = do
        ind' <- indP

        first <- kvpP
        latter <- many (expectIndP (ind' + ind) *> kvpP)

        return $ DMap (first:latter)

mapP :: Parser Document
mapP = do
  ind <- indP
  firstKvp <- kvpP
  latterKvps <- many $ expectIndP ind *> kvpP
  
  return $ DMap (firstKvp:latterKvps)
 
kvpP :: Parser (String, Document)
kvpP = (,) <$> (stringUnfencedP <|> stringLitP) <* wsOptP <*> 
  (
    (stringP ": " *> wsOptP *> simpleP <* nlOptP)
    <|> (stringP ":\n" *> nlOptP *> yamlP <* nlOptP)
  )

dashNl :: Parser ()
dashNl = () <$ stringP "-\n"

dashWs :: Parser ()
dashWs = () <$ stringP "- "