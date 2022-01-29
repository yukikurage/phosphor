{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Phosphor.Transpiler where

import           Data.Text (Text, pack, intercalate)
import           Phosphor.Data.AST (WithMetaData(WithMetaData), Literal
                                  , Literal'(LiteralString, LiteralBool, LiteralChar, LiteralFloat,
         LiteralInt), Constructor
                                  , Constructor'(Constructor), Variable
                                  , getFuncNestWithMetaData, Pattern
                                  , Pattern'(PatternVariable, PatternWildCard, PatternLiteral,
         PatternConstructor), Let, Definition
                                  , Definition'(Definition)
                                  , Let'(LetDefinition, LetReturn), Do
                                  , Do'(DoDefinition, DoReturn, DoEffect)
                                  , Expression, Expression'(ExpressionLiteral, ExpressionDo, ExpressionLet,
            ExpressionApply, ExpressionForeign, ExpressionVariable,
            ExpressionMatch, ExpressionArray)
                                  , Statement'(StatementData, StatementDefinition, StatementEnd), Statement, AST(AST)
                                  , Variable'(Variable))
import           Data.Bifunctor (Bifunctor(first))

type Transpiler a = a -> Text

withMetaDataT :: Transpiler a -> Transpiler (WithMetaData a)
withMetaDataT f (WithMetaData _ a) = f a

variableT :: Transpiler Variable
variableT (WithMetaData _ (Variable t)) = "$" <> t

-- | $$x
classVariableT :: Transpiler Text
classVariableT = ("$$" <>)

parensT :: Text -> Text
parensT a = "(" <> a <> ")"

bracesT :: Text -> Text
bracesT a = "{" <> a <> "}"

-- | name=expr;
defineT :: Text -> Text -> Text
defineT a b = a <> "=" <> b <> ";"

-- | const name=expr;
defineConstT :: Text -> Text -> Text
defineConstT a b = "const " <> a <> "=" <> b <> ";"

defineExportConstT :: Text -> Text -> Text
defineExportConstT a b = "exports." <> a <> "=" <> b <> ";"

-- | function name(args){body};
defineFunctionT :: Text -> [Text] -> Text -> Text
defineFunctionT a b c =
  "function " <> a <> "(" <> intercalate "," b <> "){" <> c <> "};"

-- | hoge_class
classT :: Text -> Text
classT a = a <> "_class"

-- | (x)=>(y)=>(z)=>expr
repLambdaT :: [Text] -> Text -> Text
repLambdaT [] a = a
repLambdaT (x:xs) a = "(" <> x <> ")=>" <> repLambdaT xs a

-- | f(a)(b)(c)
repApplyT :: Text -> [Text] -> Text
repApplyT a b = a <> "(" <> intercalate ")(" b <> ")"

-- | f(a,b,c)
applyT :: Text -> [Text] -> Text
applyT a b = a <> "(" <> intercalate "," b <> ")"

-- | return x;
returnT :: Text -> Text
returnT a = "return " <> a <> ";"

literalT :: Transpiler Literal
literalT = withMetaDataT
  $ \case
    LiteralString t -> pack $ show t
    LiteralBool a   -> if a
                       then "true"
                       else "false"
    LiteralChar a   -> pack $ show [a]
    LiteralFloat x  -> pack $ show x
    LiteralInt x    -> pack $ show x

constructorT :: Transpiler Constructor
constructorT = withMetaDataT constructorT'
  where
    constructorT'
      (Constructor v t) = classDeclaration <> makeFunctionDeclaration
      where
        classDeclaration = defineFunctionT (classT $ variableT v) args
          $ foldMap (\arg -> defineT ("this." <> arg) arg) args

        makeFunctionDeclaration = defineExportConstT
          (variableT v)
          (repLambdaT args $ "new " <> applyT (classT (variableT v)) args)
          <> defineConstT (variableT v) ("exports." <> variableT v)

        args = map
          (classVariableT . pack . show)
          [0 .. getFuncNestWithMetaData t - 1]

patternConstraints :: Pattern -> [([Int], Either Variable Literal)]
patternConstraints (WithMetaData _ a) = case a of
  PatternLiteral l -> [([], Right l)]
  PatternConstructor s ps -> ([], Left s)
    :concatMap
      (\nth -> map (first (nth:)) $ patternConstraints $ ps !! nth)
      [0 .. length ps - 1]
  _ -> []

patternVariables :: Pattern -> [([Int], Variable)]
patternVariables (WithMetaData _ a) = case a of
  PatternVariable v _ -> [([], v)]
  PatternConstructor _ ps -> concatMap
    (\nth -> map (first (nth:)) $ patternVariables $ ps !! nth)
    [0 .. length ps - 1]
  _ -> []

definitionT :: Int -> Transpiler Definition
definitionT line = withMetaDataT definitionT'
  where
    definitionT'
      (Definition p expr) = defineConstT temp (expressionT expr) <> variables
      where
        variables = foldMap
          (\(ps, v) -> defineConstT
             (variableT v)
             (temp <> foldMap (\p -> "." <> classVariableT (pack (show p))) ps))
          $ patternVariables p

        temp = "$temp_" <> pack (show line)

definitionExportT :: Int -> Transpiler Definition
definitionExportT line = withMetaDataT definitionT'
  where
    definitionT'
      (Definition p expr) = defineConstT temp (expressionT expr) <> variables
      where
        variables = foldMap
          (\(ps, v) -> defineExportConstT
             (variableT v)
             (temp <> foldMap (\p -> "." <> classVariableT (pack (show p))) ps)
           <> defineConstT (variableT v) ("exports." <> variableT v))
          $ patternVariables p

        temp = "$temp_" <> pack (show line)

effectT :: Int -> Transpiler Definition
effectT line = withMetaDataT effectT'
  where
    effectT' (Definition p expr) =
      defineConstT temp (parensT (expressionT expr) <> "()") <> variables
      where
        variables = foldMap
          (\(ps, v) -> defineConstT
             (variableT v)
             (temp <> foldMap (\p -> "." <> classVariableT (pack (show p))) ps))
          $ patternVariables p

        temp = "$temp_" <> pack (show line)

letT :: Int -> Transpiler Let
letT line = withMetaDataT letT'
  where
    letT' = \case
      LetDefinition def rest -> definitionT line def <> letT (line + 1) rest
      LetReturn expr         -> returnT $ parensT $ expressionT expr

doT :: Int -> Transpiler Do
doT line = withMetaDataT doT'
  where
    doT' = \case
      DoDefinition def rest -> definitionT line def <> doT (line + 1) rest
      DoReturn expr         -> returnT $ parensT $ expressionT expr
      DoEffect def rest     -> effectT line def <> doT (line + 1) rest

constructorsT :: [Constructor] -> Text
constructorsT = foldMap constructorT

patternT :: Int -> [Pattern] -> Expression -> Text
patternT maxNest pes e = "if("
  <> tConstraints
  <> "){"
  <> tVariables
  <> returnT (parensT $ expressionT e)
  <> foldMap (parensT . tMatch) [1 .. maxNest - length pes]
  <> "}"
  where
    tConstraints = "true"
      <> foldMap
        (\i -> foldMap
           (\(ps, c) -> case c of
              Left v  -> "&&"
                <> tMatch i
                <> foldMap (\p -> "." <> classVariableT (pack (show p))) ps
                <> " instanceof "
                <> classT (variableT v)
              Right l -> "&&"
                <> tMatch i
                <> foldMap (\p -> "." <> classVariableT (pack (show p))) ps
                <> "==="
                <> literalT l)
           (patternConstraints (pes !! i)))
        [0 .. length pes - 1]

    tVariables = foldMap
      (\i -> foldMap
         (\(ps, v) -> defineConstT (variableT v)
          $ tMatch i
          <> foldMap (\p -> "." <> classVariableT (pack (show p))) ps)
         (patternVariables (pes !! i)))
      [0 .. length pes - 1]

tMatch :: Show a => a -> Text
tMatch i = "$match" <> pack (show i)

expressionT :: Expression -> Text
expressionT = withMetaDataT expressionT'
  where
    expressionT' = \case
      ExpressionLiteral l -> literalT l
      ExpressionDo d -> "()=>{" <> doT 0 d <> "}"
      ExpressionLet l -> "(()=>{" <> letT 0 l <> "})()" --即時実行する
      ExpressionApply left right -> parensT (expressionT left)
        <> parensT (expressionT right)
      ExpressionForeign t _ -> parensT t
      ExpressionVariable v -> variableT v
      ExpressionMatch patterns -> parensT
        $ foldMap (\i -> parensT (tMatch i) <> "=>") [0 .. maxNest - 1]
        <> bracesT (foldMap (uncurry (patternT maxNest)) patterns)
        where
          maxNest = maximum $ map (\(pes, _) -> length pes) patterns
      ExpressionArray expressions _ -> foldMap expressionT expressions

statementT :: Int -> Statement -> Text
statementT line = withMetaDataT statementT'
  where
    statementT' = \case
      StatementData _ _ constructors rest -> constructorsT constructors
        <> statementT (line + 1) rest
      StatementDefinition def rest -> definitionExportT line def
        <> statementT (line + 1) rest
      StatementEnd -> ""

primitive :: Text
primitive = "function $Unit_class(){};exports.$Unit=new $Unit_class();const $Unit=exports.$Unit;"

transpile :: AST -> Text
transpile (AST ft statement) = primitive <> ft <> statementT 0 statement
