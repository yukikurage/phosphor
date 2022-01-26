module Phosphor.Data.AST where
-- コードにおける位置 エラーハンドリングに使う getOffset で取得
type CodePos = Int

type Variable = String

data AST = AST Text -- Foreign
                    [Statement]

data Statement =
                StatementDefinition CodePos Definition
        | StatementData CodePos Variable [Constructor]

data Constructor = Constructor CodePos Variable Type

data Definition = Definition CodePos Pattern Expression

data Type =
          TypeVariable CodePos Variable
        | TypeFunction CodePos Type Type

data Pattern =
    PatternVariable CodePos Variable Type
  | PatternWildCard CodePos Type
  | PatternConstructor CodePos Variable
        | PatternApply CodePos Pattern

data Expression =
    ExpressionApply CodePos Expression Expression
  | ExpressionVariable CodePos Variable Type
  | ExpressionMatch CodePos [([Pattern], Expression)]
  | ExpressionForeign CodePos Text Type
  | ExpressionLiteral CodePos Literal
  | ExpressionDo CodePos Do
  | ExpressionLet CodePos Let

data Do =
    DoReturn CodePos Expression
        | DoBind CodePos Pattern Expression Do
        | DoLet CodePos Pattern Expression Do

data Let = Let CodePos [Definition] Expression

data Literal =
    LiteralString Text
  | LiteralBool Bool
  | LiteralChar Char
  | LiteralInt Int
  | LiteralFloat Float
