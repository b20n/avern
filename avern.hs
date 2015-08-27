import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import System.IO
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

data Instruction = Call Int
                 | MoveImmediate Integer Int
                 deriving Show

-- Fun [Code] [ArgList] Argc
data RTVal = RTFun [Instruction] [Int] Int
           | RTAtom String
           | RTNumber Integer
           | RTNil
           deriving Show

data RTState = RTState [RTVal] [Instruction]
             deriving Show

type CEnv = Map String RTVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = do
  skipMany space
  char '('
  x <- liftM List $ sepBy parseExpr spaces
  char ')'
  skipMany space
  return x

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseList

-- Only definitions are valid at the toplevel
-- This is pretty messy; probably possible to define in a nicer manner.
parseTopLevel :: Parser LispVal
parseTopLevel = do
  skipMany space
  char '('
  string "define"
  skipMany space
  name <- parseAtom
  form <- parseList
  skipMany space
  char ')'
  skipMany space
  return $ List [Atom "define", name, form]

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Reimplementation of take-last
takeLeftover :: [a] -> t -> [a]
takeLeftover [] _ = []
takeLeftover (x:xss) _ = xss

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' takeLeftover xs (drop n xs)

argsToRT :: [LispVal] -> [RTVal]
argsToRT [] = []
argsToRT (Atom a : as) = RTAtom a : argsToRT as

compileForm :: LispVal -> RTState -> RTState
compileForm (Number n) (RTState regs insts) =
  RTState (RTNumber n : regs) ((MoveImmediate n (length regs)) : insts)
compileForm (List (fn:args)) state@(RTState regs _) =
  let reg = length regs
      (RTState regs' insts) = foldr compileForm state args
  in RTState (lastN (reg - 1) regs') (Call reg : insts)

lambda :: String -> [LispVal] -> CEnv -> CEnv
lambda name [(List args), body] env =
  let rtargs = argsToRT args
      regs = RTNil : rtargs
      state = RTState regs []
      RTState _ instructions = compileForm body state
      compiled = RTFun (reverse instructions) [] (length args)
  in Map.insert name compiled env

define :: [LispVal] -> CEnv -> CEnv
define (Atom name : [(List (Atom "lambda" : rest))]) env = lambda name rest env

-- At some point this might match on namespaces, or something
compile :: LispVal -> CEnv -> CEnv
compile (List (Atom "define" : rest)) env = define rest env

extractNames :: [LispVal] -> CEnv -> CEnv
extractNames [] env = env
extractNames (List [(Atom "define"), (Atom name), _]  : rest) env =
  extractNames rest (Map.insert name RTNil env)

cenv :: CEnv
cenv = Map.empty

compileTopLevel :: String -> String
compileTopLevel input = case parse (many parseTopLevel) "avern" input of
  Right parsed -> show $ foldr compile (extractNames parsed cenv) parsed
  Left err -> "No match: " ++ show err

main :: IO ()
main = do
  args <- getArgs
  forms <- readFile (args !! 0)
  putStrLn $ compileTopLevel forms
