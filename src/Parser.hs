{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Parser
  ( module Parser

  , Identifier
  ) where

import           Data.Bifunctor
import qualified Data.Set as S
import           Data.Text.Lazy (Text)
import           Data.Void
import           Lexer
import           Text.Megaparsec hiding (Token, Tokens)

data Top
  = TopDef Function
  | TopExtern Prototype
  deriving ( Show
           )

data Expr
  = ExprDouble Double
  | ExprVariable Identifier
  | ExprBinary Operator Expr Expr
  | ExprCall Identifier [Expr]
  deriving ( Show
           )

data Prototype
  = Prototype Identifier [Identifier]
  deriving ( Show
           )

data Function
  = Function Prototype Expr
  deriving ( Show
           )

data Operator
  = OpLessThan
  | OpPlus
  | OpMinus
  | OpTimes
  deriving ( Show
           )

parser :: Text -> [Either String Top]
parser = go . lexer
  where
    go ts = r : go' ts'
      where
        go' = \case
          TokEof:_        -> []
          ts''            ->
            case r of
              Left _ -> go (drop 1 ts'')
              _      -> go ts''
        (stateInput -> ts', first errorBundlePretty -> r) = runParser' parseTop initialState

        initialState = State
          { stateInput = ts
          , stateOffset = 0
          , statePosState = PosState
            { pstateInput = ts
            , pstateOffset = 0
            , pstateSourcePos = initialPos "<kaleidoscope>"
            , pstateTabWidth = mkPos 8
            , pstateLinePrefix = ""
            }
          , stateParseErrors = []
          }

-- parser :: Text -> Either (ParseErrorBundle [Token] Void) Top
-- parser = parse parseTop "<kaleidoscope>" . lexer

parseTop :: Parsec Void [Token] Top
parseTop
  = (TopDef <$> parseDefinition <|> TopExtern <$> parseExtern <|> TopDef <$> parseTopLevelExpr)
    <* single TokSemicolon

parseDefinition :: Parsec Void [Token] Function
parseDefinition
  = single TokDef
    >> Function <$> parsePrototype <*> parseExpr

parseExtern :: Parsec Void [Token] Prototype
parseExtern
  = single TokExtern
    >> parsePrototype

parsePrototype :: Parsec Void [Token] Prototype
parsePrototype = do
  fn <- token fromTokIdentifier S.empty
  args <- inParen $ many (token fromTokIdentifier S.empty)
  pure $ Prototype fn args

parseTopLevelExpr :: Parsec Void [Token] Function
parseTopLevelExpr = Function (Prototype "__anon_expr" []) <$> parseExpr

parseExpr :: Parsec Void [Token] Expr
parseExpr = do
  lhs <- parsePrimary
  parseBinaryOperator 0 lhs <|> pure lhs
  where
    parseBinaryOperator :: Int -> Expr -> Parsec Void [Token] Expr
    parseBinaryOperator prec lhs = do
      (op, opPrec) <- operatorWithPrecGT prec
      rhs <- parsePrimary
      rhs' <- parseBinaryOperator (opPrec + 1) rhs <|> pure rhs
      let
        lhs' = ExprBinary op lhs rhs'
      parseBinaryOperator prec lhs' <|> pure lhs'

parsePrimary :: Parsec Void [Token] Expr
parsePrimary = parseIdentifier <|> parseDouble <|> parseParen

parseDouble :: Parsec Void [Token] Expr
parseDouble = ExprDouble <$> token fromTokDouble S.empty

parseParen :: Parsec Void [Token] Expr
parseParen = inParen parseExpr

parseIdentifier :: Parsec Void [Token] Expr
parseIdentifier = do
  i <- token fromTokIdentifier S.empty
  ExprCall i <$> parseArgs <|> pure (ExprVariable i)
  where
    parseArgs = inParen $ sepBy parseExpr (single TokComma)

inParen :: Parsec Void [Token] a -> Parsec Void [Token] a
inParen = between (single TokOParen) (single TokCParen)

operatorWithPrecGT :: Int -> Parsec Void [Token] (Operator, Int)
operatorWithPrecGT prec = token fromOperatorToken S.empty
  where
    fromOperatorToken tok
      | Just (op, opPrec) <- operatorAndPrec tok
      , prec < opPrec
      = Just (op, opPrec)
      | otherwise
      = Nothing

operatorAndPrec :: Token -> Maybe (Operator, Int)
operatorAndPrec = \case
  TokLessThan -> Just (OpLessThan, 10)
  TokPlus     -> Just (OpPlus, 20)
  TokMinus    -> Just (OpMinus, 20)
  TokTimes    -> Just (OpTimes, 40)
  _           -> Nothing
