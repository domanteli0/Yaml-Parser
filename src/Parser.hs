-- {-# HLINT ignore "Use lambda-case" #-}
module Parser(Parser(..), Text) where
  
import Control.Applicative ( Alternative((<|>), empty) )

type Text = String
newtype Parser a = Parser
  { runParser :: Text -> Either String (a, Text)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (x, input') <- p input
      return (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (f, input') <- p1 input
      (a, input'') <- p2 input'
      return (f a, input'')

instance Alternative Parser where
  empty = Parser $ const $ Left "ERR_00: No parser"
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> 
        let out1 = p1 input
            out2 = p2 input
        in case out1 of
          Right x -> Right x
          Left _ -> out2

instance Monad Parser where
  (Parser p1) >>= f = Parser $ \input -> do
    (output, text) <- p1 input
    runParser (f output) text
