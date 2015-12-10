{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ParserCombinators where

import Control.Applicative
import Control.Monad
import Control.Monad.State

class Parses p where
    satisfy :: (Char -> Bool) -> p Char

type IsParser f
    = ( Parses f
      , Functor f
      , Applicative f
      , Alternative f
      , MonadPlus f
      , Monad f
      )

char :: Parses p => Char -> p Char
char c = satisfy (== c)

parensM :: (Parses m, Monad m) => m b -> m b
parensM p = do
    char '('
    res <- p
    char ')'
    return res

many1M :: MonadPlus p => p a -> p [a]
many1M p = do
    a <- p
    as <- manyM p
    return (a:as)

manyM :: MonadPlus p => p a -> p [a]
manyM p = many1M p `mplus` return []

option :: Alternative p => p a -> p (Maybe a)
option p = fmap Just p <|> pure Nothing

choice :: Alternative p => [p a] -> p a
choice = foldr (<|>) empty

parens :: (Parses p, Alternative p) => p b -> p b
parens p = (\_ a _ -> a) <$> char '(' <*> p <*> char ')'

many1 :: Alternative p => p a -> p [a]
many1 p = liftA2 (:) p (many p <|> pure [])

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = scan where scan = liftA2 (:) p ((s *> scan) <|> pure [])

type IsContextSensitive s p = (IsParser p, MonadState s p)

-- 1st Parser Implementation --

runParser1 :: Parser1 a -> (String -> Maybe (a, String))
runParser1 (Parser1 go) inp = go inp

newtype Parser1 a = Parser1 (String -> Maybe (a, String))

instance Functor Parser1 where
    fmap f (Parser1 go) = Parser1 $ \inp -> do
        -- using the Maybe monad here
        (a, outp) <- go inp
        return (f a, outp)

instance Applicative Parser1 where
    pure = return
    p1 <*> p2 = do
      f <- p1
      x <- p2
      return (f x)

instance Alternative Parser1 where
    empty = Parser1 (\_ -> Nothing)
    p1 <|> p2 = Parser1 $ \inp ->
        case runParser1 p1 inp of
          Nothing -> runParser1 p2 inp
          Just x -> Just x

instance Monad Parser1 where
    -- passes the input string straight through to the output
    return a = Parser1 (\inp -> return (a, inp))
    p >>= f = Parser1 $ \inp -> do
        (a, outp1) <- runParser1 p inp
        (b, outp2) <- runParser1 (f a) outp1
        return (b, outp2)

instance Parses Parser1 where
    satisfy pred = Parser1 $ \inp ->
        case inp of
            []               -> Nothing
            c:cs | pred c    -> Just (c,cs)
                 | otherwise -> Nothing

-- 2nd Parser Implementation

newtype Parser2 a =
  Parser2 (StateT String Maybe a)
  deriving (Functor, Monad, Applicative, Alternative)

runParser2 :: Parser2 a -> String -> Maybe (a, String)
runParser2 (Parser2 go) inp = runStateT go inp

instance Parses Parser2 where
    satisfy pred = Parser2 $ do
        inp <- get
        case inp of
            []               -> fail "empty input"
            c:cs | pred c    -> put cs >> return c
                 | otherwise -> fail "satisfy"

-- 3rd Parser Implementation --
-- backtracking monadic parser is a transformer stack of 'State' and '[]'

newtype Parser3 a =
    Parser3 { runParser3 :: StateT String [] a }
    deriving (Functor, Monad, Applicative, Alternative)

instance Parses Parser3 where
    satisfy pred = Parser3 $ do
        inp <- get
        case inp of
            []               -> fail "empty input"
            c:cs | pred c    -> put cs >> return c
                 | otherwise -> fail "satisfy"

-- 4th Parser Implementation --
-- 3rd implementation does depth first search, this does breadth first

newtype Parser4 m a =
    Parser4 { runParser4 :: StateT String m a }
    deriving (Functor, Monad, Applicative, Alternative)

instance FailWith m => Parses (Parser4 m) where
    satisfy pred = Parser4 $ do
        inp <- get
        case inp of
            []               -> failWith "empty input"
            c:cs | pred c    -> put cs >> return c
                 | otherwise -> failWith "satisfy"

class Monad m => FailWith m where
    failWith :: String -> m a

instance FailWith Maybe where
    failWith _ = Nothing

instance FailWith [] where
    failWith _ = []

instance FailWith m => FailWith (StateT s m) where
    failWith reason = lift (failWith reason)
