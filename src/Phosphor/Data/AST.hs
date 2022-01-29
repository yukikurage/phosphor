module Phosphor.Data.AST where

import           Data.Text (Text, unpack)

data WithMetaData a = WithMetaData Int -- Locate
                                   a
  deriving Show

instance Eq a => Eq (WithMetaData a) where
  (==) (WithMetaData _ a0) (WithMetaData _ a1) = a0 == a1

instance Ord a => Ord (WithMetaData a) where
  compare (WithMetaData _ a0) (WithMetaData _ a1) = compare a0 a1

newtype Variable' = Variable Text
  deriving (Eq, Ord, Show)

type Variable = WithMetaData Variable'

data AST = AST Text -- Foreign
               Statement
  deriving (Eq, Ord, Show)

-- getImportFiles :: AST -> [String]
-- getImportFiles (AST imports _ _) =
--   map (\(WithMetaData _ (Import t)) -> unpack t) imports
-- newtype Import' = Import Text
--   deriving (Eq, Ord, Show)
-- type Import = WithMetaData Import'
data Statement' = StatementDefinition Definition Statement
                | StatementData Variable Kind [Constructor] Statement
                | StatementEnd
  deriving (Eq, Ord, Show)

type Statement = WithMetaData Statement'

data Constructor' = Constructor Variable Type
  deriving (Eq, Ord, Show)

type Constructor = WithMetaData Constructor'

data Definition' = Definition Pattern Expression
  deriving (Eq, Ord, Show)

type Definition = WithMetaData Definition'

data Kind' = KindType
           | KindFunction Kind Kind
  deriving (Eq, Ord, Show)

type Kind = WithMetaData Kind'

data Type' = TypeVariable Variable
           | TypeConstructor Variable
           | TypeFunction Type Type
           | TypeEffect Type
           | TypeApply Type Type
  deriving (Eq, Ord, Show)

type Type = WithMetaData Type'

data Pattern' = PatternVariable Variable Type
              | PatternWildCard Type
              | PatternConstructor Variable [Pattern]
              | PatternLiteral Literal
  deriving (Eq, Ord, Show)

type Pattern = WithMetaData Pattern'

data Expression' =
    ExpressionApply Expression Expression
  | ExpressionVariable Variable
  | ExpressionMatch [([Pattern], Expression)]
  | ExpressionForeign Text Type
  | ExpressionLiteral Literal
  | ExpressionDo Do
  | ExpressionLet Let
  | ExpressionArray [Expression] Type
  deriving (Eq, Ord, Show)

type Expression = WithMetaData Expression'

data Do' = DoReturn Expression
         | DoEffect Definition Do
         | DoDefinition Definition Do
  deriving (Eq, Ord, Show)

type Do = WithMetaData Do'

data Let' = LetDefinition Definition Let
          | LetReturn Expression
  deriving (Eq, Ord, Show)

type Let = WithMetaData Let'

data Literal' = LiteralString Text
              | LiteralBool Bool
              | LiteralChar Char
              | LiteralInt Int
              | LiteralFloat Float
  deriving (Eq, Ord, Show)

type Literal = WithMetaData Literal'

getFuncNest :: Type' -> Int
getFuncNest (TypeFunction _ f) = getFuncNestWithMetaData f + 1
getFuncNest _ = 0

getFuncNestWithMetaData :: Type -> Int
getFuncNestWithMetaData (WithMetaData _ t) = getFuncNest t

getFuncParams :: Type -> ([Type], Type)
getFuncParams withMetaData@(WithMetaData _ (TypeFunction t0 t1)) = (t0:ts, t)
  where
    (ts, t) = getFuncParams t1
getFuncParams t = ([], t)

getFuncConstructor :: Type -> Maybe Variable
getFuncConstructor (WithMetaData _ (TypeConstructor v)) = Just v
getFuncConstructor (WithMetaData _ (TypeApply t0 t1)) = getFuncConstructor t0
getFuncConstructor _ = Nothing
