module Main where
import System.Environment (getArgs)
import Data.Char
import Control.Concurrent

data Tape a = Tape [a] a [a]

type State = (Tape Instruction, Tape Int, String, Int)

instance Show a => Show (Tape a) where
    show :: Show a => Tape a -> String
    show (Tape ls v rs) = printArr (reverse ls) ++ "\ESC[31m[ " ++ show v ++ " ]\ESC[0m" ++ " " ++ printArr rs
        where
            printArr :: Show a => [a] -> String
            printArr = foldr (\c acc -> show c ++ " " ++ acc) ""



instance Eq a => Eq (Tape a) where
    (==) :: Tape a -> Tape a -> Bool
    (Tape la va ra) == (Tape lb vb rb) = la == lb && va == vb && ra == rb

data Instruction = ShiftLeft
                 | ShiftRight
                 | Increment
                 | Decrement
                 | Input
                 | Output
                 | LoopStart Int
                 | LoopEnd Int
                 | End

instance Show Instruction where
    show :: Instruction -> String
    show ShiftLeft = "<"
    show ShiftRight = ">"
    show Increment = "+"
    show Decrement = "-"
    show Input = ","
    show Output = "."
    show (LoopStart _) = "["
    show (LoopEnd _) = "]"
    show End = "END"

type Parser a = String -> [(a, String)]

get :: Tape a -> a
get (Tape _ v _) = v

set :: Tape a -> a -> Tape a
set (Tape ls _ rs) v = Tape ls v rs

shiftR :: Tape a -> Tape a
shiftR (Tape ls v []      ) = Tape ls v []
shiftR (Tape ls v (r : rs)) = Tape (v:ls) r rs

shiftL :: Tape a -> Tape a
shiftL (Tape [] v rs      ) = Tape [] v rs
shiftL (Tape (l:ls) v rs) = Tape ls l (v:rs)

insertR :: Tape a -> a -> Tape a
insertR (Tape ls v rs) i = Tape (v:ls) i rs

insertL :: Tape a -> a -> Tape a
insertL (Tape ls v rs) i = Tape ls i (v:rs)

shiftInsert :: Eq a => (Tape a -> Tape a) -> (Tape a -> a -> Tape a) -> Tape a -> a -> Tape a
shiftInsert shift insert t x
    | t == shifted = insert t x
    | otherwise    = shifted
    where shifted = shift t

increment :: Num a => Tape a -> Tape a
increment t = set t (get t + 1)

decrement :: Num a => Tape a -> Tape a
decrement t = set t (get t - 1)

input :: Tape Int -> IO (Tape Int)
input t = do
    set t . read <$> getLine

output :: Show a => Tape a -> IO (Tape a)
output t = do
    print (get t)
    pure t

shiftLN :: Tape a -> Int -> Tape a
shiftLN t n = case n of
    0 -> t
    _ -> shiftLN (shiftL t) (n-1)

shiftRN :: Tape a -> Int -> Tape a
shiftRN t n = case n of
    0 -> t
    _ -> shiftRN (shiftR t) (n-1)

rewind :: Tape a -> Tape a
rewind (Tape ls v rs) = shiftLN (Tape ls v rs) (length ls)

instructionParser :: Parser Instruction
instructionParser [] = []
instructionParser (x:xs) = case x of
    '<' -> [(ShiftLeft, xs)]
    '>' -> [(ShiftRight, xs)]
    '+' -> [(Increment, xs)]
    '-' -> [(Decrement, xs)]
    '.' -> [(Output, xs)]
    ',' -> [(Input, xs)]
    '[' -> [(LoopStart 0, xs)]
    ']' -> [(LoopEnd 0, xs)]
    _   -> []

parseInstructions :: String -> Maybe (Tape Instruction)
parseInstructions str = case instructionParser str of
    [] -> Nothing
    [(x, xs)]  -> rewind <$> _parseInstructions xs 0 0 (Tape [] x [])
    where
        _parseInstructions :: String -> Int -> Int -> Tape Instruction -> Maybe (Tape Instruction)
        _parseInstructions [] loopOffset loopDelta tape
            | loopDelta == 0 = Just $ insertR tape End
            | otherwise      = Nothing
        _parseInstructions str loopOffset loopDelta tape = case instructionParser str of
            []                  -> Nothing
            [(LoopStart n, xs)] -> _parseInstructions xs 1 (loopDelta + 1) (insertR tape (LoopStart n))
            [(LoopEnd n, xs)]   -> if loopDelta < 1 then Nothing else _parseInstructions xs 1 (loopDelta - 1) (updateLoopStart (insertR tape (LoopEnd loopOffset)) loopOffset)
            [(i, xs)]           -> _parseInstructions xs (loopOffset + 1) loopDelta (insertR tape i)

        updateLoopStart :: Tape Instruction -> Int -> Tape Instruction
        updateLoopStart t offset =  shiftRN (set (shiftLN t offset) (LoopStart offset)) offset


loop :: State -> State
loop (Tape li (LoopStart x) ri, Tape ld vd rd, output, i) = case vd of
    0 -> (shiftRN (Tape li (LoopStart x) ri) x, Tape ld vd rd, output, i)
    _ -> (shiftR (Tape li (LoopStart x) ri), Tape ld vd rd, output, i)

loop (Tape li (LoopEnd x) ri, Tape ld vd rd, output, i) = case vd of
    0 -> (shiftR (Tape li (LoopEnd x) ri), Tape ld vd rd, output, i)
    _ -> (shiftLN (Tape li (LoopEnd x) ri) x, Tape ld vd rd, output, i)


executeInstruction :: State -> IO State
executeInstruction (ti, td, output, i) = do
    case get ti of
        ShiftLeft   -> return (shiftR ti, shiftInsert shiftL insertL td 0, output, i)
        ShiftRight  -> return (shiftR ti, shiftInsert shiftR insertR td 0, output, i)
        Increment   -> return (shiftR ti, increment td, output, i)
        Decrement   -> return (shiftR ti, decrement td, output, i)
        Output      -> return (shiftR ti, td, chr (get td):output, i)
        Input       -> do
            let td' = set td 0
            return (shiftR ti, td', output, i)
        LoopStart x -> return $ loop (ti, td, output, i)
        LoopEnd x   -> return $ loop (ti, td, output, i)
        End         -> return (ti, td, output, i)

executeAll :: State -> Bool -> IO State
executeAll (ti, td, output, i) slow = do
    case ti of
        Tape ls End rs -> pure (ti, td, output, i)
        _              -> do
                s <- executeInstruction (ti, td, output, i + 1)
                if slow then do
                    threadDelay 10000
                    printState (ti, td, output, i)
                else return ()

                executeAll s slow


executeAllInstructions :: String -> Bool -> IO (Maybe State)
executeAllInstructions instr slow = do
    case initState of
        Just s  -> Just <$> executeAll s slow
        Nothing -> return Nothing
    where 
        initState :: Maybe State
        initState = (, Tape [] 0 [], "", 0) <$> parseInstructions instr

printState :: State -> IO ()
printState (ti, td, output, i) = do
    putStr "\ESC[2J"
    putStrLn ""
    putStrLn $ "Output: " ++ reverse output
    putStrLn ""
    putStrLn $ "Instructions: " ++ show ti
    putStrLn ""
    putStrLn $ "Data: " ++ show td
    putStrLn ""
    putStrLn $ "Ran over " ++ show i ++ " iterations"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage ./interpreter <file.bf>"
        (filepath:flags) -> do
            file <- readFile filepath
            maybeStuff <- executeAllInstructions file (flags == ["--slow"])
            case maybeStuff of
                Just s -> printState s
                Nothing -> putStrLn "Invalid file."
