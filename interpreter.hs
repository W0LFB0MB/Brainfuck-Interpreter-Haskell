module Main where
import System.Environment (getArgs)
import Data.Char (chr, isSpace)
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)

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
                 | Error Instruction -- If you see this something is very wrong


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
    show (Error x) = "{ERR" ++ show x ++ "}"

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

shiftN :: (Tape a -> Tape a) -> Tape a -> Int -> Tape a
shiftN f t n = case n of
    0 -> t
    _ -> shiftN f (f t) (n-1)

shiftRN :: Tape a -> Int -> Tape a
shiftRN = shiftN shiftR

shiftLN :: Tape a -> Int -> Tape a
shiftLN = shiftN shiftL

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
    [(x, xs)]  -> rewind <$> _parseInstructions xs [] 0 (Tape [] x [])
    where
        _parseInstructions :: String -> [Int] -> Int -> Tape Instruction -> Maybe (Tape Instruction)
        _parseInstructions [] _ loopDelta tape
            | loopDelta == 0 = Just $ insertR tape End
            | otherwise      = Nothing
        _parseInstructions str loopOffset loopDelta tape = case instructionParser str of
            []                  -> Nothing
            [(LoopStart n, xs)] -> _parseInstructions xs (1:incLoopOffset) (loopDelta + 1) (insertRT (LoopStart n))
            [(LoopEnd n, xs)]   -> case loopDelta of
                0 -> Nothing 
                _ -> _parseInstructions xs (tail incLoopOffset) (loopDelta - 1) (updateLoopStart (insertRT (LoopEnd $ head loopOffset)))
            [(i, xs)]           -> _parseInstructions xs incLoopOffset loopDelta (insertRT i)
            where
                incLoopOffset :: [Int]
                incLoopOffset = (+1) <$> loopOffset

                insertRT :: Instruction -> Tape Instruction
                insertRT = insertR tape

        updateLoopStart :: Tape Instruction -> Tape Instruction
        updateLoopStart (Tape ls (LoopEnd x) rs) = case shiftLN (Tape ls (LoopEnd x) rs) x of
            (Tape ls' (LoopStart _) rs') -> shiftRN (Tape ls' (LoopStart x) rs') x
            (Tape ls' v' rs')            -> shiftRN (Tape ls' (Error v') rs') x
        updateLoopStart (Tape ls v rs) = Tape ls (Error v) rs


loop :: State -> State
loop (Tape li (LoopStart x) ri, td, output, i) = case vd of
    0 -> (shiftRN (Tape li (LoopStart x) ri) x, td, output, i)
    _ -> (shiftR (Tape li (LoopStart x) ri), td, output, i)
    where (Tape ld vd rd) = td

loop (Tape li (LoopEnd x) ri, td, output, i) = case vd of
    0 -> (shiftR (Tape li (LoopEnd x) ri), td, output, i)
    _ -> (shiftLN (Tape li (LoopEnd x) ri) x, td, output, i)
    where (Tape ld vd rd) = td

safeChrCons :: Int -> String -> String
safeChrCons x str
    | x < 0     = str
    | otherwise = chr x : str

executeInstruction :: State -> IO State
executeInstruction (ti, td, output, i) = do
    case get ti of
        ShiftLeft   -> return (shiftR ti, shiftInsert shiftL insertL td 0, output, i)
        ShiftRight  -> return (shiftR ti, shiftInsert shiftR insertR td 0, output, i)
        Increment   -> return (shiftR ti, increment td, output, i)
        Decrement   -> return (shiftR ti, decrement td, output, i)
        Output      -> return (shiftR ti, td, safeChrCons (get td) output, i)
        Input       -> do
            putStr "Enter number: "
            inp <- readMaybe <$> getLine
            case inp of
                Nothing -> return (shiftR ti, set td 0, output, i)
                Just x  -> return (shiftR ti, set td x, output, i)
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
                    printState s
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
    putStrLn $ "Ran over \ESC[31m" ++ show i ++ "\ESC[0m iterations"

    -- -- Fancier print, breaks when output contains newlines
    -- putStr "\ESC[2J"
    -- putStr $ "\ESC[5A\rOutput: " ++ reverse output
    -- putStr $ "\ESC[2B\rInstructions: " ++ show ti 
    -- putStr $ "\ESC[2B\rData: " ++ show td 
    -- putStr $ "\ESC[2B\rRan over \ESC[31m" ++ show i ++ "\ESC[0m iterations"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage ./interpreter <file.bf>"
        (filepath:flags) -> do
            file <- readFile filepath
            let filteredFile = filter (not . isSpace) file
            maybeStuff <- executeAllInstructions filteredFile (flags == ["--slow"])
            case maybeStuff of
                Just s -> do
                    printState s
                    putStrLn ""
                Nothing -> putStrLn "Invalid file."
