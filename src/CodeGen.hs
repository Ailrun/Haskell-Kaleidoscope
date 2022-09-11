{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.FloatingPointPredicate as AST
import qualified LLVM.AST.Global as AST
import qualified LLVM.AST.Linkage as AST
import qualified LLVM.AST.Type as AST
import           LLVM.Context
import           LLVM.IRBuilder
import           LLVM.Module
import           Parser

codeGen :: [Top] -> IO Text
codeGen ts = withContext $ \c -> do
  m <- buildModuleT "my cool jit" $ foldM genTop Map.empty ts
  withModuleFromAST c m (fmap (T.decodeUtf8 . B.fromStrict) . moduleLLVMAssembly)

genTop :: (MonadFail m, MonadModuleBuilder m)
       => Map Identifier AST.Operand
       -> Top
       -> m (Map Identifier AST.Operand)
genTop gm (TopDef fun@(Function (Prototype f _) _)) = do
  v <- genFunction gm fun
  pure $ Map.insert f v gm
genTop gm (TopExtern p@(Prototype f _)) = do
  v <- genPrototype p
  pure $ Map.insert f v gm

genExpr :: (MonadFail m, MonadIRBuilder m)
        => Map Identifier AST.Operand
        -> Map Identifier AST.Operand
        -> Expr
        -> m AST.Operand
genExpr _ _ (ExprDouble d) = pure $ double d
genExpr _ lm (ExprVariable i)
  | Just v <- lm Map.!? i
  = pure v
  | otherwise
  = fail "Unknown variable name"
genExpr gm lm (ExprBinary op l r) = do
  lv <- genExpr gm lm l
  rv <- genExpr gm lm r
  case op of
    OpLessThan -> do
      ltv <- named (fcmp AST.ULT lv rv) "cmptmp"
      named (uitofp ltv AST.double) "booltmp"
    OpPlus -> named (fadd lv rv) "addtmp"
    OpMinus -> named (fsub lv rv) "subtmp"
    OpTimes -> named (fmul lv rv) "multmp"
genExpr gm lm (ExprCall f args)
  | Just fv <- gm Map.!? f
  , AST.ConstantOperand (AST.GlobalReference (AST.PointerType (AST.FunctionType _ argsty _) _) _) <- fv
  = if length argsty == length args
    then do
      argsv <- traverse (genExpr gm lm) args
      named (call fv [(argv, []) | argv <- argsv]) "calltmp"
    else
      fail "Incorrect # arguments passed"
  | otherwise
  = fail "Unknown function referenced"

genPrototype :: (MonadModuleBuilder m) => Prototype -> m AST.Operand
genPrototype proto@(Prototype f args) = do
  emitDefn . AST.GlobalDefinition $ globalFromPrototype proto
  pure . AST.ConstantOperand . AST.GlobalReference fty $ AST.mkName (T.unpack f)
  where
    fty = AST.ptr $ AST.FunctionType AST.double [AST.double | _ <- args] False

-- |
-- This one contains the bug that prototype of a function in the same module
-- may cause a conflict with the function itself (as there's no easy way to
-- "update" an emitted definition)
genFunction :: (MonadFail m, MonadModuleBuilder m) => Map Identifier AST.Operand -> Function -> m AST.Operand
genFunction gm (Function proto@(Prototype f args) body) = do
  (_, blocks) <- runIRBuilderT emptyIRBuilder $ do
    emitBlockStart "entry"
    v <- genExpr gm' lm body
    ret v
  emitDefn . AST.GlobalDefinition $ (globalFromPrototype proto)
    { AST.basicBlocks = blocks
    }
  pure fv
  where
    gm' = Map.insert f fv gm
    fv = AST.ConstantOperand . AST.GlobalReference fty $ AST.mkName (T.unpack f)
    lm = Map.fromList [(arg, AST.LocalReference AST.double (AST.mkName (T.unpack arg))) | arg <- args]
    fty = AST.ptr $ AST.FunctionType AST.double [AST.double | _ <- args] False

globalFromPrototype :: Prototype -> AST.Global
globalFromPrototype (Prototype f args)
  = AST.functionDefaults
    { AST.name        = AST.mkName (T.unpack f)
    , AST.linkage     = AST.External
    , AST.parameters  = ([AST.Parameter AST.double (AST.mkName (T.unpack arg)) [] | arg <- args], False)
    , AST.returnType  = AST.double
    }
