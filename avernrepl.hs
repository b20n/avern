import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

import Data.Map (Map)
import qualified Data.Map as Map

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

-- VM

data Instruction = Call Int
                 | MoveImmediate Integer Int
                 deriving Show

data Fun = Fun [Instruction] [RTVal] Int
         deriving Show

data RTVal = RTFun Fun
           | RTAtom String
           | RTNumber Integer
           | RTString String
           | RTBool Bool
           | RTNil
           | RTRef -- TODO
           deriving Show

type RTEnv = Map String RTVal

data RTFrame = RTFrame Int Int Fun
             deriving Show

data RT = RT [RTVal] [RTFrame]
        deriving Show

nextInstruction :: [RTFrame] -> Instruction
nextInstruction ((RTFrame pc _ (Fun insts _ _)):_) = insts !! pc

base :: [RTFrame] -> Int
base ((RTFrame _ base _):_) =  base

deref :: RTEnv -> RTVal -> RTVal
deref env _ = RTNil

-- Horrible, but don't want to bother with arrays atm
setElement :: Int -> RTVal -> [RTVal] -> [RTVal]
setElement n e xs = take n xs ++ [e] ++ drop (n + 1) xs

incrementPC :: [RTFrame] -> [RTFrame]
incrementPC ((RTFrame pc base fn):frames) = (RTFrame (pc + 1) base fn) : frames

run :: RTEnv -> [RTVal] -> [RTFrame] -> RTVal
run env (r:rs) [] = r
run env registers stack = case nextInstruction stack of
  Call reg -> let RTFun fun = deref env $ registers !! reg
                  frame = RTFrame 0 (base stack) fun
              in run env registers (frame : stack)
  MoveImmediate val reg -> let i = RTNumber val
                               registers' = setElement reg i registers
                               stack' = incrementPC stack
                           in run env registers' stack'

-- Parser

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

compileLambda :: RTEnv -> [LispVal] -> LispVal -> (RTEnv, RTVal)
compileLambda env args form = (env, RTNil)

eval :: RTEnv -> LispVal -> (RTEnv, RTVal)
eval env (String val) = (env, RTString val)
eval env (Number val) = (env, RTNumber val)
eval env (Bool val) = (env, RTBool val)
eval env (Atom val) = case Map.lookup val env of
  Just val' -> (env, val')
  Nothing -> (env, RTNil)
--eval env (List [Atom "quote", val]) = (env, RTAtom val)
eval env (List [Atom "lambda", List args, form]) = compileLambda env args form
eval env (List [Atom "define", Atom name, form]) = let
  (_, form') = eval env form
  in (Map.insert name form' env, form')

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- evalString :: String -> IO String
-- evalString expr = return $ show . eval $ readExpr expr

evalAndPrint :: RTEnv -> String -> IO RTEnv
evalAndPrint env expr =
  let (env', result) = eval env (readExpr expr)
  in do
    putStrLn $ show result
    return env'

until_ :: Monad m => (a -> Bool) -> m a -> (RTEnv -> a -> m RTEnv) -> RTEnv -> m RTEnv
until_ pred prompt action env = do
     result <- prompt
     if pred result
       then return env
       else action env result >>= until_ pred prompt action

runRepl :: IO RTEnv
runRepl = until_ (== "quit") (readPrompt "avn> ") evalAndPrint rtenv

rtenv :: RTEnv
rtenv = Map.empty

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl >> return ()
            otherwise -> putStrLn "Program takes only 0 or 1 argument"
