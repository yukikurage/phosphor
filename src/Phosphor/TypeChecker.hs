{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Phosphor.TypeChecker where

import qualified Data.List.NonEmpty as NonEmpty
import           Phosphor.Data.AST (Kind, Type, Expression
                                  , WithMetaData(WithMetaData), Variable
                                  , Type'(TypeVariable, TypeConstructor, TypeFunction, TypeEffect,
      TypeApply)
                                  , Kind'(KindType, KindFunction), Literal
                                  , Literal'(LiteralInt, LiteralBool, LiteralChar, LiteralFloat,
         LiteralString)
                                  , Variable'(Variable), Constructor
                                  , Constructor'(Constructor), getFuncNest
                                  , getFuncParams, getFuncConstructor
                                  , Expression'(ExpressionVariable, ExpressionDo, ExpressionLet,
            ExpressionLiteral, ExpressionArray, ExpressionForeign,
            ExpressionMatch, ExpressionApply), Pattern
                                  , Pattern'(PatternVariable, PatternWildCard, PatternConstructor,
         PatternLiteral), Definition
                                  , Definition'(Definition), Let
                                  , Let'(LetReturn, LetDefinition), Do
                                  , Do'(DoReturn, DoEffect, DoDefinition)
                                  , Statement, Statement'(StatementDefinition, StatementData, StatementEnd), AST(AST))
import           Control.Monad.State (State, get, put, runState, gets
                                    , MonadTrans(lift), StateT(runStateT)
                                    , modify, when)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack)
import           Data.Bifunctor (Bifunctor(second, first))
import           Text.Megaparsec
import qualified Data.Set as S
import           Data.Void (Void)
import           Debug.Trace (traceM)

data TypeCheckError =
    MismatchKind Kind' Kind' Type
  | MismatchType Type' Type' Expression
  | DuplicatedType Variable
  | DuplicatedVariable Variable
  | InsufficientPatternMatch Expression
  | EmptyPatternMatch Int
  | UnknownType Variable
  | UnknownVariable Variable
  | OtherError String Int
  deriving (Eq, Show, Ord)

instance ShowErrorComponent TypeCheckError where
  showErrorComponent = show

  errorComponentLen _ = 1

getOffsetFromTypeCheckError :: TypeCheckError -> Int
getOffsetFromTypeCheckError = \case
  MismatchKind _ _ (WithMetaData o _) -> o
  MismatchType _ _ (WithMetaData o _) -> o
  DuplicatedType (WithMetaData o _) -> o
  DuplicatedVariable (WithMetaData o _) -> o
  InsufficientPatternMatch (WithMetaData o _) -> o
  EmptyPatternMatch o -> o
  UnknownType (WithMetaData o _) -> o
  UnknownVariable (WithMetaData o _) -> o
  OtherError _ o -> o

toErrorBundle
  :: Text -> [TypeCheckError] -> ParseErrorBundle Text TypeCheckError
toErrorBundle source ts =
  ParseErrorBundle { bundleErrors = NonEmpty.fromList
                       $ map
                         (\t -> FancyError (getOffsetFromTypeCheckError t)
                          $ S.singleton
                          $ ErrorCustom t)
                         ts
                   , bundlePosState = initialState
                   }
  where
    initialState = PosState { pstateInput = source
                            , pstateOffset = 0
                            , pstateSourcePos = initialPos ""
                            , pstateTabWidth = defaultTabWidth
                            , pstateLinePrefix = ""
                            }

data TypeCheckState = TypeCheckState { typeMap :: Map Variable Kind'
                                     , variableMap :: Map Variable Type'
                                     }
  deriving (Eq, Show)

type TypeCheckerM x = StateT TypeCheckState (Either [TypeCheckError]) x

type TypeChecker a x = a -> TypeCheckerM x

getTypeKind :: TypeChecker Variable (Maybe Kind')
getTypeKind v = gets (M.lookup v . typeMap)

getTypeKindWithError :: TypeChecker Variable Kind'
getTypeKindWithError v = do
  m <- getTypeKind v
  case m of
    Just k  -> pure k
    Nothing -> lift $ Left [UnknownType v]

addTypeKind :: Variable -> Kind' -> TypeCheckerM ()
addTypeKind v k = do
  look <- getTypeKind v
  case look of
    Nothing -> modify (\s -> s { typeMap = M.insert v k (typeMap s) })
    _       -> lift $ Left [DuplicatedType v]

getVariableType :: TypeChecker Variable (Maybe Type')
getVariableType v = gets (M.lookup v . variableMap)

getVariableTypeWithError :: TypeChecker Variable Type'
getVariableTypeWithError v = do
  look <- getVariableType v
  case look of
    Nothing -> lift $ Left [UnknownType v]
    Just t  -> pure t

addVariableType :: Variable -> Type' -> TypeCheckerM ()
addVariableType v t = do
  look <- getVariableType v
  case look of
    Nothing -> modify (\s -> s { variableMap = M.insert v t (variableMap s) })
    _       -> lift $ Left [DuplicatedVariable v]

withMetaDataC :: TypeChecker a x -> TypeChecker (WithMetaData a) x
withMetaDataC f (WithMetaData _ x) = f x

unMetaData :: WithMetaData a -> a
unMetaData (WithMetaData _ x) = x

getMetaData :: WithMetaData a -> Int
getMetaData (WithMetaData x _) = x

typeC :: TypeChecker Type Kind'
typeC = withMetaDataC typeC'
  where
    typeC' = \case
      TypeConstructor v  -> do
        k <- getTypeKindWithError v
        lift $ Right k
      TypeVariable v     -> lift $ Left [UnknownVariable v]
      TypeFunction t1 t2 -> do
        k1 <- typeC t1
        k2 <- typeC t2
        lift
          $ if k1 == KindType && k2 == KindType
            then Right k1
            else Left
              [MismatchKind KindType k1 t1, MismatchKind KindType k2 t2]
      TypeEffect t       -> do
        k <- typeC t
        lift
          $ if k == KindType
            then Right k
            else Left [MismatchKind KindType k t]
      TypeApply t1 t2    -> do
        k1 <- typeC t1
        k2 <- typeC t2
        case k1 of
          KindFunction (WithMetaData _ k3) (WithMetaData _ k4)
            | k3 == k2 -> pure k4
          _ -> lift
            $ Left [MismatchKind KindType k1 t1, MismatchKind KindType k2 t2]

isKindType :: TypeChecker Type ()
isKindType t = do
  k <- typeC t
  if k == KindType
    then return ()
    else lift $ Left [MismatchKind KindType k t]

mkVariable :: Text -> Variable
mkVariable str = WithMetaData 0 $ Variable str

literalC :: TypeChecker Literal Type'
literalC = withMetaDataC literalC'
  where
    mkLiteralType str = TypeConstructor $ WithMetaData 0 $ Variable str

    literalC' = \case
      LiteralInt _    -> pure $ mkLiteralType "Int"
      LiteralBool _   -> pure $ mkLiteralType "Bool"
      LiteralChar _   -> pure $ mkLiteralType "Char"
      LiteralFloat _  -> pure $ mkLiteralType "Float"
      LiteralString _ -> pure $ mkLiteralType "String"

constructorC :: Variable -> TypeChecker Constructor ()
constructorC typeName = withMetaDataC constructorC'
  where
    constructorC' (Constructor v t) = do
      isKindType t
      let cons = getFuncConstructor $ snd $ getFuncParams t
          o = getMetaData v
      if cons == Just typeName
        then do
          addVariableType v $ unMetaData t
        else lift
          $ Left
            [ MismatchType
                (TypeConstructor typeName)
                (TypeConstructor
                 $ fromMaybe (WithMetaData o $ Variable "") cons)
                (WithMetaData o $ ExpressionVariable v)]

patternC :: TypeChecker Pattern Type'
patternC (WithMetaData offset a) = patternC' a
  where
    patternC' = \case
      PatternVariable v t -> do
        isKindType t
        addVariableType v $ unMetaData t
        pure $ unMetaData t
      PatternWildCard t -> do
        isKindType t
        pure $ unMetaData t
      PatternConstructor c patterns -> do
        t <- getVariableTypeWithError c
        let (args, result) = getFuncParams $ WithMetaData 0 t
        if length patterns == length args
          then do
            mapM_
              (\(expectedArg, pattern) -> do
                 actualArg <- patternC pattern
                 if expectedArg == actualArg
                   then return ()
                   else lift
                     $ Left
                       [ MismatchType
                           expectedArg
                           actualArg
                           (WithMetaData 0 $ ExpressionVariable c)])
              $ zip (map unMetaData args) patterns
            pure $ unMetaData result
          else do
            expectedKind <- typeC $ WithMetaData 0 t
            lift $ Left [OtherError "Error in PatternConstructor" offset]
      PatternLiteral l -> literalC l

definitionC :: TypeChecker Definition ()
definitionC = withMetaDataC definitionC'
  where
    definitionC' (Definition pattern e) = do
      left <- patternC pattern
      right <- expressionC e
      if left == right
        then return ()
        else lift $ Left [MismatchType left right e]

effectC :: TypeChecker Definition ()
effectC = withMetaDataC effectC'
  where
    effectC' (Definition pattern e) = do
      left <- patternC pattern
      right <- expressionC e
      if TypeEffect (WithMetaData 0 left) == right
        then return ()
        else lift $ Left [MismatchType left right e]

letC :: TypeChecker Let Type'
letC = withMetaDataC letC'
  where
    letC' = \case
      LetReturn e -> do
        t <- expressionC e
        isKindType $ WithMetaData 0 t
        pure t
      LetDefinition def rest -> do
        definitionC def
        letC rest

doC :: TypeChecker Do Type'
doC = withMetaDataC doC'
  where
    doC' = \case
      DoReturn e -> do
        t <- expressionC e
        isKindType $ WithMetaData 0 t
        pure $ TypeEffect $ WithMetaData 0 t
      DoEffect eff rest -> do
        effectC eff
        doC rest
      DoDefinition def rest -> do
        definitionC def
        doC rest

constructorsC :: Variable -> TypeChecker [Constructor] ()
constructorsC v = mapM_ $ constructorC v

patternMatchC :: TypeChecker ([Pattern], Expression) Type'
patternMatchC (patterns, e) = do
  state <- get
  patternTypes <- mapM patternC patterns
  expressionType <- expressionC e
  put state -- スコープを切る
  pure
    $ foldr
      (\t acc -> TypeFunction (WithMetaData 0 t) (WithMetaData 0 acc))
      expressionType
      patternTypes

expressionC :: TypeChecker Expression Type'
expressionC (WithMetaData offset a) = expressionC' a
  where
    expressionC' = \case
      ExpressionVariable v -> getVariableTypeWithError v
      ExpressionDo d -> do
        state <- get
        t <- doC d
        isKindType $ WithMetaData 0 t
        put state -- スコープを切る
        pure t
      ExpressionLet l -> do
        state <- get
        t <- letC l
        isKindType $ WithMetaData 0 t
        put state -- スコープを切る
        pure t
      ExpressionLiteral l -> literalC l
      ExpressionArray expressions arrayType -> do
        isKindType arrayType
        mapM_
          (\expression -> do
             t <- expressionC expression
             if TypeApply
               (WithMetaData 0
                $ TypeConstructor
                $ WithMetaData 0
                $ Variable "Array")
               (WithMetaData 0 t)
               == unMetaData arrayType
               then return ()
               else lift
                 $ Left [MismatchType (unMetaData arrayType) t expression])
          expressions
        pure $ unMetaData arrayType
      ExpressionApply e1 e2 -> do
        t1 <- expressionC e1
        t2 <- expressionC e2
        case t1 of
          TypeFunction t3 t4
            | unMetaData t3 == t2 -> pure $ unMetaData t4
          t -> lift $ Left [OtherError "Error in ExpressionApply" offset]
      ExpressionForeign _ t -> do
        isKindType t
        pure $ unMetaData t
      ExpressionMatch [] -> lift $ Left [EmptyPatternMatch offset]
      ExpressionMatch ((hPattern, hExpression):restPattern) -> do
        headT <- patternMatchC (hPattern, hExpression)
        mapM_
          (\pattern -> do
             t <- patternMatchC pattern
             if headT == t
               then return ()
               else lift $ Left [MismatchType headT t hExpression])
          restPattern
        pure headT

statementC :: TypeChecker Statement ()
statementC = withMetaDataC statementC'
  where
    statementC' = \case
      StatementDefinition def rest -> do
        definitionC def
        statementC rest
      StatementData dataName kind constructors rest -> do
        addTypeKind dataName $ unMetaData kind
        constructorsC dataName constructors
        statementC rest
      StatementEnd -> pure ()

astC :: TypeChecker AST ()
astC (AST _ statement) = statementC statement

primitiveState :: TypeCheckState
primitiveState =
  TypeCheckState { typeMap = M.fromList
                     $ map
                       (first mkVariable)
                       [ ("Int", KindType)
                       , ("Bool", KindType)
                       , ("String", KindType)
                       , ("Unit", KindType)
                       , ( "Array"
                         , KindFunction
                             (WithMetaData 0 KindType)
                             (WithMetaData 0 KindType))
                       , ("Char", KindType)
                       , ("Float", KindType)]
                 , variableMap = M.fromList
                     $ map
                       (first mkVariable)
                       [ ( "Unit"
                         , TypeConstructor (WithMetaData 0 $ Variable "Unit"))]
                 }

typeCheck :: AST -> Maybe [TypeCheckError]
typeCheck ast = case runStateT (astC ast) primitiveState of
  Left errors -> Just errors
  Right _     -> Nothing
