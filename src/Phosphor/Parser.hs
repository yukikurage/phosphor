{-# LANGUAGE OverloadedStrings #-}

module Phosphor.Parser where

import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec (Parsec, between, manyTill, (<|>), getOffset
                                , many, sepBy1, MonadParsec(try, eof), manyTill_
                                , empty)
import           Data.Void
import qualified Data.Text as T
import           Text.Megaparsec.Char (space1, char, letterChar, alphaNumChar
                                     , upperChar, lowerChar)
import           Text.Megaparsec.Char.Lexer (charLiteral)
import           Data.Functor (($>))
import           Phosphor.Data.AST (WithMetaData(WithMetaData), Literal
                                  , Literal'(LiteralChar, LiteralString, LiteralBool, LiteralInt,
         LiteralFloat), Type
                                  , Type'(TypeFunction, TypeVariable, TypeEffect), Variable, Constructor
                                  , Constructor'(Constructor), Pattern
                                  , Pattern'(PatternConstructor, PatternVariable, PatternWildCard,
         PatternLiteral), Expression
                                  , Expression'(ExpressionForeign, ExpressionLiteral,
            ExpressionVariable, ExpressionApply, ExpressionDo, ExpressionLet,
            ExpressionMatch), Definition
                                  , Definition'(Definition), Let
                                  , Let'(LetDefinition, LetReturn), Do
                                  , Do'(DoDefinition, DoReturn, DoEffect)
                                  , Statement, Statement'(StatementDefinition, StatementEnd, StatementData), AST(AST))

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

signedFloat :: Parser Float
signedFloat = L.signed (pure ()) float

signedInteger :: Parser Int
signedInteger = L.signed (pure ()) integer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

backQuotes :: Parser T.Text
backQuotes = T.pack <$> (symbol "`" *> manyTill L.charLiteral (symbol "`"))

doubleQuotes :: Parser T.Text
doubleQuotes = T.pack <$> (symbol "\"" *> manyTill L.charLiteral (symbol "\""))

bool :: Parser Bool
bool = (symbol "true" $> True) <|> (symbol "false" $> False)

withMetaData :: Parser a -> Parser (WithMetaData a)
withMetaData p = WithMetaData <$> getOffset <*> p

parser :: Parser AST
parser = AST <$> (backQuotes <|> pure "") <*> statementP

statementP :: Parser Statement
statementP = withMetaData statementP'
  where
    statementP' = statementEndP <|> statementDataP <|> statementDefinitionP

    statementDefinitionP = StatementDefinition <$> definitionP <*> statementP

    statementDataP = do
      symbol "data "
      s <- upperVariableP
      symbol "["
      res <- many constructorP
      symbol "]"
      symbol ";"
      StatementData s res <$> statementP

    statementEndP = eof $> StatementEnd

literalP :: Parser Literal
literalP = withMetaData literalP'
  where
    literalP' = pChar <|> pString <|> pBoolean <|> pInt <|> pNumber

    pChar = lexeme
      $ LiteralChar <$> between (symbol "\'") (symbol "\'") charLiteral

    pString = LiteralString <$> doubleQuotes

    pBoolean = LiteralBool <$> bool

    pInt = LiteralInt <$> signedInteger

    pNumber = LiteralFloat <$> signedFloat

typeP :: Parser Type
typeP = withMetaData typeP'
  where
    typeP' = typeEffect <|> typeFunctionP <|> typeVariableP

    typeEffect = TypeEffect <$> braces typeP

    typeFunctionP = try
      $ do
        t0 <- parens typeP <|> withMetaData typeVariableP
        symbol "->"
        TypeFunction t0 <$> typeP

    typeVariableP = TypeVariable <$> upperVariableP

constructorP :: Parser Constructor
constructorP = withMetaData constructorP'
  where
    constructorP' =
      Constructor <$> upperVariableP <* symbol ":" <*> typeP <* symbol ";"

patternP :: Parser Pattern
patternP = withMetaData patternP'
  where
    patternP' = patternWildCardP
      <|> patternConstructorP
      <|> patternVariableP
      <|> patternLiteralP

    patternConstructorP = do
      cons <- upperVariableP
      res <- parens $ sepBy1 patternP $ (symbol "," <|> symbol ")(")
      pure $ PatternConstructor cons res

    patternVariableP =
      PatternVariable <$> lowerVariableP <* symbol ":" <*> typeP

    patternWildCardP = symbol "_" *> symbol ":" *> (PatternWildCard <$> typeP)

    patternLiteralP = PatternLiteral <$> literalP

expressionP :: Parser Expression
expressionP = withMetaData expressionP'
  where
    expressionP' = parens expressionP'
      <|> expressionForeignP
      <|> expressionDoP
      <|> expressionLetP
      <|> try expressionMatchP
      <|> expressionLiteralP
      <|> expressionApplyP

    expressionForeignP =
      ExpressionForeign <$> backQuotes <* symbol ":" <*> typeP

    expressionLiteralP = ExpressionLiteral <$> literalP

    expressionVariableP = ExpressionVariable <$> variableP

    expressionApplyP = applyP
      ExpressionApply
      (expressionVariableP <|> parens expressionP')
      expressionP'

    expressionLetP = symbol "let" *> braces (ExpressionLet <$> letP)

    expressionDoP = symbol "do" *> braces (ExpressionDo <$> doP)

    expressionMatchP = expressionMatchManyP <|> expressionMatchMonoP

    expressionMatchMonoP = do
      ps <- sepBy1 patternP (symbol ",")
      symbol "->"
      e <- expressionP
      return $ ExpressionMatch [(ps, e)]

    expressionMatchManyP = do
      symbol "["
      res <- many
        $ do
          ps <- sepBy1 patternP (symbol ",")
          symbol "->"
          e <- expressionP
          symbol ";"
          return (ps, e)
      symbol "]"
      return $ ExpressionMatch res

letP :: Parser Let
letP = withMetaData letP'
  where
    letP' = letReturnP <|> letDefinitionP

    letDefinitionP = LetDefinition <$> definitionP <*> letP

    letReturnP = symbol "return " *> (LetReturn <$> expressionP) <* symbol ";"

doP :: Parser Do
doP = withMetaData doP'
  where
    doP' = doReturnP <|> try doEffectP <|> doDefinitionP

    doDefinitionP = DoDefinition <$> definitionP <*> doP

    doEffectP = DoEffect <$> effectP <*> doP

    doReturnP = symbol "return " *> (DoReturn <$> expressionP) <* symbol ";"

returnP :: Parser Expression
returnP = symbol "return " *> expressionP <* symbol ";"

definitionP :: Parser Definition
definitionP = withMetaData definitionP'
  where
    definitionP' =
      Definition <$> patternP <* symbol "=" <*> expressionP <* symbol ";"

effectP :: Parser Definition
effectP = withMetaData effectP'
  where
    effectP' =
      Definition <$> patternP <* symbol "<-" <*> expressionP <* symbol ";"

-- | ex)
-- |   p
-- |   a(b)(c)
-- |   a(b, c)  -- equal to a(b)(c)
-- |   (p(a))(b)
applyP :: (WithMetaData a -> WithMetaData a -> a)
       -> Parser a
       -> Parser a
       -> Parser a
applyP f left right = do
  o <- getOffset
  v <- left
  xs <- concat <$> many (parens $ sepBy1 (withMetaData right) (symbol ","))
  pure $ foldl (f . WithMetaData o) v xs

variableP :: Parser Variable
variableP = T.pack
  <$> ((:) <$> (char '_' <|> letterChar) <*> many (alphaNumChar <|> char '_'))

upperVariableP :: Parser Variable
upperVariableP = lexeme
  $ (\x -> if x `elem` reservedWords
           then empty
           else pure x)
  . T.pack
  =<< ((:) <$> (char '_' <|> upperChar) <*> many (alphaNumChar <|> char '_'))

lowerVariableP :: Parser Variable
lowerVariableP = lexeme
  $ (\x -> if x `elem` reservedWords
           then empty
           else pure x)
  . T.pack
  =<< ((:) <$> (char '_' <|> lowerChar) <*> many (alphaNumChar <|> char '_'))

reservedWords :: [T.Text]
reservedWords = ["return", "let", "do", "true", "false", "data"]
