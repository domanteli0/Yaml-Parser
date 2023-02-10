{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

module Common(
    charP
  , stringP
  , readP
  , indP
  , expectIndP
  , nlP
  , optP
  , optMP
  , fencedP
  , wsOptP
  , takeWhileP
  , nonEmpty
  , nonConsumingP
  , expectP
  , nlOptP
  , toListP
  , overrideErrP
) where

import Parser as P
import Control.Applicative as A
import Text.Read (readEither)

readP :: (Read a) => String -> Parser a
readP str = Parser $ \input -> do 
  ret <- readEither str
  return (ret, input)

errorP :: String -> P.Parser a
errorP err = P.Parser $ \input -> Left $ err ++ "; with text left: " ++ show input

charP :: Char -> Parser Char
charP ch = P.Parser f
  where
    f [] = Left $ "No input left, expected: " ++ show ch
    f (ch':str)
      | ch == ch' = Right (ch', str)
      | otherwise = Left $ "expected: " ++ show ch ++ ", got: " ++ show (ch':str)

stringP :: String -> P.Parser String
stringP = mapM charP

indP :: Parser Int
indP = length <$> many (charP ' ')

-- |takes an Int and failes if not the exact amount of space characters is parsed
expectIndP :: Int -> Parser Int
expectIndP i = expectP indP (== i)

nlP :: Parser Char
nlP = charP '\n'

nlOptP :: Parser [Char]
nlOptP = optP nlP

wsOptP :: Parser ()
wsOptP = () <$ many (charP ' ')

fencedP :: Char -> Parser P.Text
fencedP ch = fencedDiffP ch ch

fencedDiffP :: Char -> Char -> Parser P.Text
fencedDiffP ch1 ch2 = Parser $ \input -> do 
  (_, t) <- runParser (charP ch1) input
  let (str, text) = break (== ch2) t
  text' <- safeTail text
  return (str, text')

safeTail :: [a] -> Either String [a]
safeTail [] = Left "Empty list"
safeTail (_:xs) = Right xs

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP p = Parser $ \input -> Right (span p input)

nonEmpty :: Parser [a] -> Parser [a]
nonEmpty p = p >>= (\a -> if null a then errorP "expected nonEmpty" else return a)

-- |Takes a parser and wraps the parsed result into a list
toListP :: Parser a -> Parser [a]
toListP = (<$>) (: [])

-- parser modifiers --
-- |Modifies a parser such that if a second argument (a -> Bool) returns false error is throw
expectP :: Parser a -> (a -> Bool) -> Parser a
expectP p pred' = do
  a <- p
  if pred' a 
    then return a
    else errorP "condition unmet"

-- |If succesfull list of one `a` is parsed else an empty list
optP :: Parser a -> Parser [a]
optP (Parser p) = Parser $ \input ->
  case p input of
    Left _ -> Right ([], input)
    Right (a, t) -> Right ([a], t)

optMP :: Parser a -> Parser (Maybe a)
optMP (Parser p) = Parser $ \input ->
  case p input of
    Left _ -> Right (Nothing, input)
    Right (a, t) -> Right (Just a, t)

-- |Takes a parsed and makes it so that it doesn't consume any input
nonConsumingP :: Parser a -> Parser a
nonConsumingP (Parser p) = 
  Parser $ \input -> do
    (a, _) <- p input
    return (a, input) 

overrideErrP :: Parser a -> String -> Parser a
overrideErrP (Parser p) err  = Parser $ \input ->
  case p input of
    Left _ -> Left err
    Right a -> Right a
