module Main where

-- Type for the Name of the Variable 
type Ident = String

data ByteCode = LOAD_VAL Int
              | WRITE_VAR Ident
              | READ_VAR Ident
              | ADD
              | SUB
              | MULTIPLY
              | DIVIDE
              | RETURN_VALUE
              deriving Show

data Value = NumVal Int | Stack Env
  deriving (Show)

-- Base type for the stored entity/Variable in the stack 
type Entity = (Ident, Int)

-- Stack Environment for storing the entities/Variables in the ByteCode
type Env = [Entity]

-- Push function for the Stack Environment
push :: Env -> Entity -> Env
push env e = e : env

-- Pop function for the Stack Environment
pop :: Env -> Env
pop = tail

-- Top value of the Stack Environment
peek :: Env -> Entity
peek = head

runByteCode :: ByteCode -> Env -> Value
runByteCode (LOAD_VAL i) env = load env i
runByteCode (WRITE_VAR i) env = write env i
runByteCode (READ_VAR i) env = readVar env i
runByteCode ADD env = add env
runByteCode SUB env = sub env
runByteCode MULTIPLY env = mul env
runByteCode DIVIDE env = divVar env
runByteCode RETURN_VALUE env = returnVal env

-- Load the given to the environmet
load :: Env -> Int -> Value
load env i = Stack $ push env ("None", i)

-- Renaming the loaded value 
write :: Env -> Ident -> Value
write env i = Stack $ push (pop env) (i, snd $ peek env)

-- Reads the value for the given variable name in the stack
readVar :: Env -> Ident -> Value
readVar env i = Stack $ push env $ find env i

-- Finds the entity with the given name of the variable in the environment
find :: Env -> Ident -> Entity
find env i = head $ filter (\(i', _) -> i == i') env

-- Adds the last two variables in the stack and push the result
add :: Env -> Value
add env = Stack $ push (pop env) ("None", snd (peek (pop env)) + snd (peek env))

-- Substracts the last variable from the second last variable in the stack and push the result
sub :: Env -> Value
sub env = Stack $ push (pop env) ("None", snd (peek (pop env)) - snd (peek env))

-- Multiplies the last two variables in the stack and push the result
mul :: Env -> Value
mul env = Stack $ push (pop env) ("None", snd (peek (pop env)) * snd (peek env))

-- Divides the second last variable with the last variable in the stack and push the result
divVar :: Env -> Value
divVar env = Stack $ push (pop env) ("None", snd (peek (pop env)) `div` snd (peek env))

-- Returns the value of the top Entity of the stack
returnVal :: Env -> Value
returnVal env = NumVal (snd $ peek env)

main :: IO ()
main = putStrLn "Hello, Haskell!"
