{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Lexer where

import           Data.Char
import           Data.Functor
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import           Data.Void
import           Text.Megaparsec (TraversableStream (..), VisualStream (..))

type Identifier = Text

data Token
  = TokEof
  | TokDef
  | TokExtern
  | TokIdentifier Text
  | TokDouble Double
  | TokOParen
  | TokCParen
  | TokComma
  | TokSemicolon
  | TokLessThan
  | TokPlus
  | TokMinus
  | TokTimes
  | TokOther Char
  deriving ( Eq
           , Ord
           , Show
           )

instance VisualStream [Token] where
  showTokens _ = show

instance TraversableStream [Token] where
  reachOffsetNoLine _ = id

fromTokIdentifier :: Token -> Maybe Text
fromTokIdentifier (TokIdentifier i) = Just i
fromTokIdentifier _                 = Nothing

fromTokDouble :: Token -> Maybe Double
fromTokDouble (TokDouble d) = Just d
fromTokDouble _             = Nothing

isBinaryOperator :: Token -> Bool
isBinaryOperator = \case
  TokLessThan -> True
  TokPlus     -> True
  TokMinus    -> True
  TokTimes    -> True
  _           -> False

lexer :: Text -> [Token]
lexer t@(T.uncons -> Just (c, cs))
  | isSpace c
  = lexer cs
  | c == '#'
  = lexer (removeComment cs)
  | isAlpha c
  , (cs', t') <- T.span isAlphaNum cs
  = ident2tok (T.cons c cs') : lexer t'
  | isDigit c
  , Right (d, t') <- T.rational t
  = TokDouble d : lexer t'
  | c == '.'
  , Right (d, t') <- T.rational (T.cons '0' t)
  = TokDouble d : lexer t'
  | c == '('
  = TokOParen : lexer cs
  | c == ')'
  = TokCParen : lexer cs
  | c == ','
  = TokComma : lexer cs
  | c == ';'
  = TokSemicolon : lexer cs
  | c == '<'
  = TokLessThan : lexer cs
  | c == '+'
  = TokPlus : lexer cs
  | c == '-'
  = TokMinus : lexer cs
  | c == '*'
  = TokTimes : lexer cs
  | otherwise
  = TokOther c : lexer cs
  where
    removeComment = T.drop 1 . T.dropWhile ((/= LineSeparator) . generalCategory)

    ident2tok = \case
      "extern" -> TokExtern
      "def"    -> TokDef
      cs       -> TokIdentifier cs
lexer _ = repeat TokEof
