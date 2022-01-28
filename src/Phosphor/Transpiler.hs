{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Phosphor.Transpiler where

import           Data.Text (Text, pack, intercalate)
import           Phosphor.Data.AST (WithMetaData(WithMetaData), Literal
                                  , Literal'(LiteralString, LiteralBool, LiteralChar, LiteralFloat,
         LiteralInt), Constructor
                                  , Constructor'(Constructor), Variable
                                  , getFuncNestWithMetaData, Pattern
                                  , Pattern'(PatternVariable, PatternWildCard, PatternLiteral,
         PatternConstructor))
import           Data.Bifunctor (Bifunctor(first))

type Transpiler a = a -> Text

withMetaDataT :: Transpiler a -> Transpiler (WithMetaData a)
withMetaDataT f (WithMetaData _ a) = f a

variableT :: Transpiler Variable
variableT = ("$" <>)

-- | $$x
tempVariableT :: Transpiler Text
tempVariableT = ("$$" <>)

parensT :: Text -> Text
parensT a = "(" <> a <> ")"

bracesT :: Text -> Text
bracesT a = "{" <> a <> "}"

-- | name=expr;
defineT :: Text -> Text -> Text
defineT a b = a <> "=" <> b <> ";"

-- | name=expr;
defineConstT :: Text -> Text -> Text
defineConstT a b = "const " <> a <> "=" <> b <> ";"

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

literalT :: Transpiler Literal
literalT = withMetaDataT literalT'
  where
    literalT' = \case
      LiteralString t -> "\"" <> t <> "\""
      LiteralBool a   -> if a
                         then "true"
                         else "false"
      LiteralChar a   -> "\"" <> pack [a] <> "\""
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

        makeFunctionDeclaration = defineConstT (variableT v)
          $ repLambdaT args
          $ "new " <> applyT (classT (variableT v)) args

        args = map
          (tempVariableT . pack . show)
          [0 .. getFuncNestWithMetaData t - 1]

patternConstraints :: Pattern -> [([Int], Either Variable Literal)]
patternConstraints (WithMetaData _ a) = case a of
  PatternVariable s _ -> []
  PatternWildCard _ -> []
  PatternLiteral l -> [([], Right l)]
  PatternConstructor s ps -> ([], Left s):child
    where
      child = concatMap
        (\nth -> map (first (nth:)) $ patternConstraints $ ps !! nth)
        [0 .. length ps - 1]
-- patternVariables :: Pattern -> [([Int], Variable)]
