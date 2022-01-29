{-# LANGUAGE OverloadedStrings #-}

module Phosphor.Parser where

import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec (Parsec, between, manyTill, (<|>), getOffset
                                , many, sepBy1, MonadParsec(try, eof), manyTill_
                                , empty, sepBy, some)
import           Data.Void
import qualified Data.Text as T
import           Text.Megaparsec.Char (space1, char, letterChar, alphaNumChar
                                     , upperChar, lowerChar)
import           Text.Megaparsec.Char.Lexer (charLiteral)
import           Data.Functor (($>))
import           Phosphor.Data.AST (WithMetaData(WithMetaData), Literal
                                  , Literal'(LiteralChar, LiteralString, LiteralBool, LiteralInt,
         LiteralFloat), Type
                                  , Type'(TypeFunction, TypeVariable, TypeEffect, TypeConstructor,
      TypeApply), Variable, Constructor
                                  , Constructor'(Constructor), Pattern
                                  , Pattern'(PatternConstructor, PatternVariable, PatternWildCard,
         PatternLiteral), Expression
                                  , Expression'(ExpressionForeign, ExpressionLiteral,
            ExpressionVariable, ExpressionApply, ExpressionDo, ExpressionLet,
            ExpressionMatch, ExpressionArray), Definition
                                  , Definition'(Definition), Let
                                  , Let'(LetDefinition, LetReturn), Do
                                  , Do'(DoDefinition, DoReturn, DoEffect)
                                  , Statement, Statement'(StatementDefinition, StatementEnd, StatementData), AST(AST), Kind
                                  , Kind'(KindType, KindFunction)
                                  , Variable'(Variable))
import           Text.Megaparsec.Debug (dbg)

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
parser = AST <$> {-many importP <*>-} (backQuotes <|> pure "") <*> statementP

-- importP :: Parser Import
-- importP = withMetaData importP'
--   where
--     importP' = symbol "import " *> (Import <$> doubleQuotes) <* symbol ";"
kindP :: Parser Kind
kindP = withMetaData kindP'
  where
    kindP' = parens kindP' <|> kindFunction <|> kindType

    kindType = symbol "*" $> KindType

    kindFunction = try
      $ do
        t0 <- parens kindP <|> withMetaData kindType
        symbol "=>"
        KindFunction t0 <$> kindP

statementP :: Parser Statement
statementP = withMetaData statementP'
  where
    statementP' = statementEndP <|> statementDataP <|> statementDefinitionP

    statementDefinitionP = StatementDefinition <$> definitionP <*> statementP

    statementDataP = do
      symbol "data "
      s <- upperVariableP
      symbol ":"
      k <- kindP
      symbol "["
      res <- many constructorP
      symbol "]"
      symbol ";"
      StatementData s k res <$> statementP

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
    typeP' = typeEffect <|> typeFunctionP <|> typeApplyP

    typeEffect = TypeEffect <$> braces typeP

    typeFunctionP = try
      $ do
        t0 <- withMetaData typeApplyP
        symbol "=>"
        TypeFunction t0 <$> typeP

    typeConstructorP = TypeConstructor <$> upperVariableP

    typeApplyP = applyP
      TypeApply
      (parens typeP' <|> typeConstructorP <|> typeVariableP)
      typeP'

    typeVariableP = TypeVariable <$> lowerVariableP

constructorP :: Parser Constructor
constructorP = withMetaData constructorP'
  where
    constructorP' =
      Constructor <$> upperVariableP <* symbol ":" <*> typeP <* symbol ";"

patternP :: Parser Pattern
patternP = withMetaData patternP'
  where
    patternP' = parens patternP'
      <|> patternWildCardP
      <|> patternConstructorP
      <|> patternVariableP
      <|> patternLiteralP

    patternConstructorP = do
      cons <- upperVariableP
      res <- concat <$> many (parens $ sepBy1 patternP (symbol ","))
      pure $ PatternConstructor cons res

    patternVariableP =
      PatternVariable <$> lowerVariableP <* symbol ":" <*> typeP

    patternWildCardP = symbol "_" *> symbol ":" *> (PatternWildCard <$> typeP)

    patternLiteralP = PatternLiteral <$> literalP

expressionP :: Parser Expression
expressionP = withMetaData expressionP'
  where
    expressionP' = expressionForeignP
      <|> try expressionMatchMonoP
      <|> expressionLiteralP
      <|> try expressionArrayP
      <|> expressionApplyP

    expressionForeignP =
      ExpressionForeign <$> backQuotes <* symbol ":" <*> typeP

    expressionLiteralP = ExpressionLiteral <$> literalP

    expressionVariableP = ExpressionVariable <$> variableP

    expressionApplyP = applyP
      ExpressionApply
      (expressionVariableP
       <|> try (parens expressionP')
       <|> try expressionMatchManyP
       <|> expressionDoP
       <|> expressionLetP)
      expressionP'

    expressionLetP = symbol "let" *> braces (ExpressionLet <$> letP)

    expressionDoP = symbol "do" *> braces (ExpressionDo <$> doP)

    expressionMatchP = expressionMatchManyP <|> expressionMatchMonoP

    expressionArrayP = ExpressionArray
      <$> brackets (sepBy expressionP (symbol ","))
      <* symbol ":"
      <*> typeP

    expressionMatchMonoP = do
      ps <- parens $ sepBy1 patternP (symbol ",")
      symbol "=>"
      e <- expressionP
      return $ ExpressionMatch [(ps, e)]

    expressionMatchManyP = do
      symbol "["
      res <- some
        $ do
          ps <- parens $ sepBy1 patternP (symbol ",")
          symbol "=>"
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
    doP' = try doEffectP <|> try doDefinitionP <|> doReturnP <|> withOutReturn

    doDefinitionP = DoDefinition <$> definitionP <*> doP

    doEffectP = DoEffect <$> effectP <*> doP

    doReturnP = symbol "return " *> (DoReturn <$> expressionP) <* symbol ";"

    withOutReturn = do
      o <- getOffset
      pure
        $ DoReturn
        $ WithMetaData o
        $ ExpressionVariable
        $ WithMetaData o
        $ Variable "Unit"

returnP :: Parser Expression
returnP = symbol "return " *> expressionP <* symbol ";"

definitionP :: Parser Definition
definitionP = withMetaData definitionP'
  where
    definitionP' =
      Definition <$> patternP <* symbol "=" <*> expressionP <* symbol ";"

effectP :: Parser Definition
effectP = withMetaData $ try effectP' <|> withOutBind
  where
    effectP' =
      Definition <$> patternP <* symbol "=<" <*> expressionP <* symbol ";"

    withOutBind = do
      o <- getOffset
      Definition
        (WithMetaData o
         $ PatternWildCard
         $ WithMetaData o
         $ TypeConstructor
         $ WithMetaData o
         $ Variable "Unit")
        <$> expressionP
        <* symbol ";"

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
variableP = withMetaData
  $ Variable
  <$> try
    (lexeme
     $ (\x -> if x `elem` reservedWords
              then empty
              else pure x)
     . T.pack
     =<< ((:) <$> (char '_' <|> letterChar)
          <*> many (alphaNumChar <|> char '_')))

upperVariableP :: Parser Variable
upperVariableP = withMetaData
  $ Variable
  <$> try
    (lexeme
     $ (\x -> if x `elem` reservedWords
              then empty
              else pure x)
     . T.pack
     =<< ((:) <$> upperChar <*> many (alphaNumChar <|> char '_')))

lowerVariableP :: Parser Variable
lowerVariableP = withMetaData
  $ Variable
  <$> try
    (lexeme
     $ (\x -> if x `elem` reservedWords
              then empty
              else pure x)
     . T.pack
     =<< ((:) <$> (char '_' <|> lowerChar)
          <*> many (alphaNumChar <|> char '_')))

reservedWords :: [T.Text]
reservedWords = ["return", "let", "do", "true", "false", "data", "import"]
