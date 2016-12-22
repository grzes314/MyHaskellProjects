import Control.Applicative hiding (many)
import Char

type ParserFun a = String -> [(a,String)]
newtype Parser a = Parser { parse :: ParserFun a }

result :: a -> Parser a
result v = Parser $ \inp -> [(v,inp)]

zero :: Parser a
zero = Parser $ \inp -> []

item :: Parser Char
item = Parser $ \inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)]

greedy :: Parser a -> Parser a
greedy p = Parser $ \inp ->
    let pairs = parse p inp
    in case pairs of
        [] -> []
        x:xs -> [x]

satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p phi = Parser $ \inp ->
    let pairs = parse p inp
        pairOK (a, _) = phi a
    in filter pairOK pairs

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p =
        let f' (a, out) = (f a, out)
        in Parser $ \inp -> map f' (parse p inp)

instance Applicative Parser where
    pure = result
    -- <*> :: Parser (a->b) -> Parser a -> Parser b
    p <*> q = Parser $ \inp ->
        let pairs = parse p inp
            --appOnePair :: (a->b, String) -> [(b,String)]
            appOnePair (f,out) = parse (fmap f q) out
        in concat $ map appOnePair pairs

instance Alternative Parser where
    empty = zero
    (Parser p) <|> (Parser q) = Parser $ \inp -> (p inp ++ q inp)

sat :: (Char -> Bool) -> Parser Char
sat = satisfy item

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit


string :: String -> Parser String
string "" = result ""
string (x:xs) = (:) <$> (char x) <*> (string xs)

word :: Parser String
word = aux <|> result "" where
    aux = (:) <$> letter <*> word

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> (many p)) <|> result []

ident :: Parser String
ident = (:) <$> lower <*> (many alphanum)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> (many p)

nat :: Parser Int
nat = read <$> (many1 digit)

int :: Parser Int
int = (char '-' *> (negate <$> nat)) <|> nat

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = (:) <$> p <*> (many psep) where
    psep = sep *> p

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> (result [])

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = open *> p <* close

-- przyklad
intArray :: Parser [Int]
intArray = bracket (char '{') (sepby int (char ',')) (char '}')

-- Usuwanie białych znaków

spaces :: Parser ()
spaces = many1 (sat Char.isSpace) *> (result ())

comment :: Parser ()
comment = string "--" *>
          many (sat (\x -> x /= '\n')) *>
          result ()

junk :: Parser ()
junk = greedy (many ((greedy spaces) <|> comment)) *>
       result ()

clear :: Parser a -> Parser a
clear p = junk *> p

natural :: Parser Int
natural = clear nat

integer :: Parser Int
integer = clear int

symbol:: String -> Parser String
symbol xs = clear (string xs)

identifier :: [String] -> Parser String
identifier ks = clear $ satisfy ident (`notElem` ks)

inBrackets :: Parser a -> Parser b -> Parser c -> Parser b
inBrackets open p close = bracket (clear open) (clear p) (clear close)

-- prosty parser wyrazen
expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainl1` multop

factor :: Parser Int
factor = baseOrExp `chainr1` expop

baseOrExp :: Parser Int
baseOrExp = natural <|> inBrackets (char '(') expr (char ')')

parseOp :: String -> fun -> Parser fun
parseOp str fun = clear $ string str *> result fun

parseOps :: [(String, fun)] -> Parser fun
parseOps = foldr (\(s,f) acc -> (parseOp s f) <|> acc) zero

addop = parseOps [("+", (+)),
                  ("-", (-))]

multop = parseOps [("*", (*)),
                  ("/", (div))]

expop = parseOp "**" (^)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = appL <$> p <*> (many sepp) where
    appL a fs = foldl (.) id fs a
    sepp = (flip <$> op) <*> p

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = appR <$> (many psep) <*> p where
        appR = foldr (.) id
        psep = flip ($) <$> p <*> op


-- parse expr " 8 / 2 / 2 "
-- parse expr "  2  ** 3   **  2  "
-- parse expr "  2   ** 3  -  (   5   -  3    **   2 )   "

