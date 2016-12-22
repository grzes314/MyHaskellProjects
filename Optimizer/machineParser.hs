{-
  Grzegorz Łoś
  Moduł zawiera parser wczytujący maszynę stanów.
-}

module MachineParser (
  module Machine,
  parseChar
) where

import Machine
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Error
import Text.Parsec.Language

def = emptyDef{ identStart = letter <|> char '_'
              , identLetter = alphaNum <|> char '_'
              , reservedNames = ["IF", "DECISION", "CASE",
                                 "ELSEIF", "ARM", "EQAULS",
                                 "AND", "OR", "VAR"]
              }

TokenParser{ parens = m_parens
             , identifier = m_identifier
             , reserved = m_reserved
             , stringLiteral = m_string
             , integer = m_integer
             , whiteSpace = m_whiteSpace } = makeTokenParser def

type InputName = String
type Code = String

{- funkcja parseChar przyjmuje nazwę stringa z kodem (przydatne do generowania
   komunikatół o błędach), kod (czyli po prostu string), a zwraca Left err,
   gdzie err to string z komunikatem błędu, gdy parsowanie nie powiodło się,
   Right char, gdzie char to wczytana maszyna stanów.
-}
parseChar :: InputName -> Code -> Either String Character 
parseChar name code = case parse (m_whiteSpace >> characterParser <* eof) name code of
  { Left err -> Left $ show err
  ; Right char -> Right char
  }

characterParser :: Parser Character
characterParser = m_parens (fmap Character (many ruleParser))

ruleParser :: Parser Rule
ruleParser = m_parens ( do {
                states <- many stateNumParser;
                stmt <- stmtParser;
                return $ Rule states stmt
              } )

stmtParser :: Parser Stmt
stmtParser = m_parens (
             do { m_reserved "IF";
                  cond <- condParser;
                  thenn <- stmtParser;
                  elseifs <- m_parens (many elseIfParser);;
                  elss <- stmtParser;
                  return $ If cond thenn elseifs elss }
         <|> do { m_reserved "DECISION";
                  ns <- newStateParser;
                  msg <- utteranceParser;
                  return $ Dec ns msg }
         <|> do { m_reserved "CASE";
                  var <- varParser;
                  arms <- m_parens (many armParser);
                  elss <- stmtParser;
                  return $ Case var arms elss }
          )

elseIfParser :: Parser ElseIf
elseIfParser = m_parens ( do {
                 m_reserved "ELSEIF";
                 cond <- condParser;
                 thenn <- stmtParser;
                 return $ ElseIf cond thenn }
               )
                 
armParser :: Parser Arm
armParser = m_parens ( do {
               m_reserved "ARM";
               vset <- valueSetParser;
               stmt <- stmtParser;
               return $ Arm vset stmt }
            )

condParser :: Parser Cond
condParser = m_parens (
             do { m_reserved "EQUALS";
                  var <- varParser;
                  val <- m_integer;
                  return $ Eq var val }
         <|> do { m_reserved "AND";
                  conds <- many condParser;
                  return $ And conds }
         <|> do { m_reserved "OR";
                  conds <- many condParser;
                  return $ Or conds }
          )

varParser :: Parser Var
varParser = m_parens ( do {
              m_reserved "VAR";
              str <- m_string;
              return $ Var str }
            )

stateNumParser :: Parser StateNum
stateNumParser = m_integer

newStateParser :: Parser NewState
newStateParser = (char '_' >> m_whiteSpace >> return Nothing)
             <|> (stateNumParser >>= (\num -> return $ Just num))
             <?> "state number or '_'"

utteranceParser :: Parser Utterance
utteranceParser = m_string

valueSetParser :: Parser ValueSet
valueSetParser = m_parens (many stateNumParser)


   
