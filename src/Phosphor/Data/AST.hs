module Phosphor.Data.AST where

import           Data.Text (Text)

data WithMetaData a = WithMetaData Int -- Locate
                                   a
  deriving Show

instance Eq a => Eq (WithMetaData a) where
  (==) (WithMetaData _ a0) (WithMetaData _ a1) = a0 == a1

instance Ord a => Ord (WithMetaData a) where
  compare (WithMetaData _ a0) (WithMetaData _ a1) = compare a0 a1

type Variable = Text

data AST = AST Text -- Foreign
               Statement
  deriving (Eq, Ord, Show)

data Statement' = StatementDefinition Definition Statement
                | StatementData Variable [Constructor] Statement
                | StatementEnd
  deriving (Eq, Ord, Show)

type Statement = WithMetaData Statement'

data Constructor' = Constructor Variable Type
  deriving (Eq, Ord, Show)

type Constructor = WithMetaData Constructor'

data Definition' = Definition Pattern Expression
  deriving (Eq, Ord, Show)

type Definition = WithMetaData Definition'

data Type' = TypeVariable Variable
           | TypeConstructor Variable
           | TypeFunction Type Type
           | TypeEffect Type
  deriving (Eq, Ord, Show)

type Type = WithMetaData Type'

data Pattern' = PatternVariable Variable Type
              | PatternWildCard Type
              | PatternConstructor Variable [Pattern]
              | PatternLiteral Literal
  deriving (Eq, Ord, Show)

type Pattern = WithMetaData Pattern'

data Expression' = ExpressionApply Expression Expression
                 | ExpressionVariable Variable
                 | ExpressionMatch [([Pattern], Expression)]
                 | ExpressionForeign Text Type
                 | ExpressionLiteral Literal
                 | ExpressionDo Do
                 | ExpressionLet Let
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
