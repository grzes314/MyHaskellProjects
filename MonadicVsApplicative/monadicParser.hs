import Control.Applicative hiding (many)
import Control.Monad
import Char

{-------------------------------------------------------------------------------
--- Definicja parsera. Parsery "atomowe". --------------------------------------
--- Poniższy fragment można pozostawić nienaruszony, można dopisać parser ------
--- satisfy :: Parser a -> (a -> Bool) -> Parser a -----------------------------
-------------------------------------------------------------------------------}

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


{-------------------------------------------------------------------------------
--- Składanie parserów, parser jako monada z plusem. Poniższy kod należy -------
--- skasować i uczynić Parser instancją klas Functor, Applicative i Alternative.
-------------------------------------------------------------------------------}

bind :: ParserFun a -> (a -> ParserFun b) -> ParserFun b
bind p f = \inp -> concat [f v out | (v,out) <- p inp]

instance Monad Parser where
    -- return :: a -> Parser a
    return = result
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ bind p (parse . f)

instance MonadPlus Parser where
    -- mzero :: Parser a
    mzero = zero
    -- mplus :: Parser a -> Parser a -> Parser a
    (Parser p) `mplus` (Parser q) = Parser $ \inp -> (p inp ++ q inp)

infixl 3 <+>
(<+>) = mplus


{-------------------------------------------------------------------------------
---- Więcej parserów. Od tego miejsca nie wolno korzystać ze struktury ---------
---- parsera, tzn. nie wolno pisać "Parser $ \inp ->". -------------------------
---- Trzeba zmienić styl monadyczny na aplikatywny. ----------------------------
-------------------------------------------------------------------------------}

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then result x
           else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = lower <+> upper

alphanum :: Parser Char
alphanum = letter <+> digit

string :: String -> Parser String
string "" = result ""
string (x:xs) = do
    char x
    string xs
    result (x:xs)

word :: Parser String
word = aux <+> result "" where
    aux = do
        x <- letter
        xs <- word
        return (x:xs)

many :: Parser a -> Parser [a]
many p = do
        x <- p
        xs <- many p
        return (x:xs)
    <+> result []

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    result (x:xs)


many1 :: Parser a -> Parser [a]
many1 p = do
        x <- p
        xs <- many p
        return (x:xs)

nat :: Parser Int
nat = liftM read (many1 digit)

int :: Parser Int
int = do {_ <- char '-'; n <- nat; return (-n)} <+> nat

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
    open
    res <- p
    close
    return res

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
    x <- p
    xs <- many (do
        sep
        y <- p
        return y )
    return (x:xs)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <+> (result [])

-- przyklad: parser tablicy liczb
intArray :: Parser [Int]
intArray = bracket (char '{') (sepby int (char ',')) (char '}')
-- parse intArray {12,-34,5,-67,8}


{-------------------------------------------------------------------------------
------------------------ Usuwanie bialych znakow -------------------------------
-------------------------------------------------------------------------------}

spaces :: Parser ()
spaces = do
    many1 (sat Char.isSpace)
    return ()

comment :: Parser ()
comment = do
    string "--"
    many (sat (\x -> x /= '\n'))
    return ()

junk :: Parser ()
junk = do
    greedy $ many ((greedy spaces) <+> comment)
    return ()

clear :: Parser a -> Parser a
clear p = junk >> p

natural :: Parser Int
natural = clear nat

integer :: Parser Int
integer = clear int

symbol:: String -> Parser String
symbol xs = clear (string xs)

identifier :: [String] -> Parser String
identifier ks = clear $ do
    x <- ident
    if (elem x ks)
        then zero
        else return x

inBrackets :: Parser a -> Parser b -> Parser c -> Parser b
inBrackets open p close = bracket (clear open) (clear p) (clear close)


{-------------------------------------------------------------------------------
------------------------ Prosty parser wyrażeń ---------------------------------
------------ Również wymaga dostosowania do stylu aplikatywnego ----------------
-------------------------------------------------------------------------------}

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainl1` multop

factor :: Parser Int
factor = baseOrExp `chainr1` expop

baseOrExp :: Parser Int
baseOrExp = integer <+> inBrackets (char '(') expr (char ')')

parseOp :: String -> fun -> Parser fun
parseOp str fun = clear $ (string str >> return fun)

parseOps :: [(String, fun)] -> Parser fun
parseOps = foldr (\(s,f) acc -> (parseOp s f) <+> acc) zero

addop = parseOps [("+", (+)),
                  ("-", (-))]

multop = parseOps [("*", (*)),
                  ("/", (div))]

expop = parseOp "**" (^)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
    x <- p
    fs <- many (do
        f <- op
        y <- p
        return (flip f y))
    return $ foldl (.) id fs x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do
    fs <- many (do
        y <- p
        f <- op
        return (f y))
    x <- p
    return $ foldr (.) id fs x


