module Main where
import System.Environment (getArgs)
import Data.Char (chr, isSpace)
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)

data Tape a = Tape [a] a [a]

instance Functor Tape where
    fmap :: (a -> b) -> Tape a -> Tape b
    fmap f (Tape ls m rs) = Tape (f <$> ls) (f m) (f <$> rs)

instance Applicative Tape where
    pure :: a -> Tape a
    pure x = Tape [] x []

    (<*>) :: Tape (a -> b) -> Tape a -> Tape b
    ft <*> t = get ft <$> t

type State = (Tape Instruction, Tape Int, String, [Int], Int)

instance Show a => Show (Tape a) where
    show :: Show a => Tape a -> String
    show (Tape ls v rs) = printArr (reverse ls) ++ "[ " ++ show v ++ " ] " ++ printArr rs

tapeToPrettyString :: Show a => Tape a -> String
tapeToPrettyString (Tape ls v rs) = printArr (reverse ls) ++ red ("[ " ++ show v ++ " ] ") ++ printArr rs

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
    deriving Eq


instance Show Instruction where
    show :: Instruction -> String
    show ShiftLeft     = "<"
    show ShiftRight    = ">"
    show Increment     = "+"
    show Decrement     = "-"
    show Input         = ","
    show Output        = "."
    show (LoopStart i) = "["
    show (LoopEnd i)   = "]"
    show End           = "END"

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

increment1B :: Tape Int -> Tape Int
increment1B (Tape ls v rs)
    | v == 127 = Tape ls (-128) rs
    | otherwise = increment (Tape ls v rs)

decrement1B :: Tape Int -> Tape Int
decrement1B (Tape ls v rs)
    | v == -128 = Tape ls 127 rs
    | otherwise = decrement (Tape ls v rs)

byteify :: Int -> Int
byteify x
    | masked > 127 = masked - 255
    | otherwise = masked
    where 
        masked :: Int
        masked = sum $ uncurry (*) <$> zip ((2^) <$> [0..7]) (getBits x 8)

getBits :: Int -> Int -> [Int]
getBits x y = getBit x <$> [0..(y-1)]
        
getBit :: Int -> Int -> Int
getBit x y = (x `div` 2 ^ y) `mod` 2

(>>>) :: Tape a -> Int -> Tape a
t >>> i = repeatf i shiftR t
infixr 8 >>>

(<<<) :: Tape a -> Int -> Tape a
t <<< i = repeatf i shiftL t
infixr 8 <<<

repeatf :: Int -> (a -> a) -> a -> a
repeatf 0 _ = id
repeatf n f = repeatf (n - 1) f . f

lexInstructions :: [Char] -> [Instruction]
lexInstructions [] = []
lexInstructions (x:xs) = case x of
    '<' -> ShiftLeft : lexInstructions xs
    '>' -> ShiftRight : lexInstructions xs
    '+' -> Increment : lexInstructions xs
    '-' -> Decrement : lexInstructions xs
    '.' -> Output : lexInstructions xs
    ',' -> Input : lexInstructions xs
    '[' -> LoopStart 0 : lexInstructions xs
    ']' -> LoopEnd 0 : lexInstructions xs
    _   -> lexInstructions xs

rewind :: Tape a -> Tape a
rewind (Tape ls v rs) = Tape ls v rs <<< length ls

parseInstructions :: [Instruction] -> Maybe (Tape Instruction)
parseInstructions is = rewind <$> parseInstructions' is [] (pure End)
    where
        parseInstructions' :: [Instruction] -> [Int] -> Tape Instruction -> Maybe (Tape Instruction)
        parseInstructions' []     [ ] t = Just t
        parseInstructions' []     ___ t = Nothing
        parseInstructions' (LoopStart n:is) off t = parseInstructions' is (1 : inc off) $ t <+ LoopStart n
        parseInstructions' (LoopEnd _:_) [] t = Nothing
        parseInstructions' (LoopEnd n:is) off t = parseInstructions' is (inc $ tail off) $ (t <+ LoopEnd (head off)) <! head off
        parseInstructions' (i:is) off t = parseInstructions' is (inc off) $ t <+ i

        (<+) :: Tape a -> a -> Tape a
        (<+) t v = insertL t v >>> 1

        (<!) :: Tape Instruction -> Int -> Tape Instruction
        (<!) t i = Tape ls (LoopStart i) rs >>> (i + 1)
            where (Tape ls _ rs) = t <<< (i + 1)

        inc :: [Int] -> [Int]
        inc = fmap (+1)


loop :: State -> State
loop (Tape li (LoopStart x) ri, td, output, input, i) = case vd of
    0 -> (Tape li (LoopStart x) ri >>> x, td, output, input, i)
    _ -> (Tape li (LoopStart x) ri >>> 1, td, output, input, i)
    where (Tape ld vd rd) = td

loop (Tape li (LoopEnd x) ri, td, output, input, i) = case vd of
    0 -> (Tape li (LoopEnd x) ri >>> 1, td, output, input, i)
    _ -> (Tape li (LoopEnd x) ri <<< x, td, output, input, i)
    where (Tape ld vd rd) = td

safeChrCons :: Int -> String -> String
safeChrCons x str
    | x < 0     = str
    | otherwise = chr x : str

executeInstruction :: State -> State
executeInstruction (ti, td, output, [x], i) = executeInstruction (ti >>> 1, set td x, output, [], i)
executeInstruction (ti, td, output, input, i) =
    case get ti of
        ShiftLeft   -> (ti >>> 1, shiftInsert (<<< 1) insertL td 0, output, input, i)
        ShiftRight  -> (ti >>> 1, shiftInsert (>>> 1) insertR td 0, output, input, i)
        Increment   -> (ti >>> 1, increment1B td, output, input, i)
        Decrement   -> (ti >>> 1, decrement1B td, output, input, i)
        Output      -> (ti >>> 1, td, safeChrCons (get td) output, input, i)
        Input       -> (ti, td, output, [0], i)
        LoopStart x -> loop (ti, td, output, input, i)
        LoopEnd x   -> loop (ti, td, output, input, i)
        End         -> (ti, td, output, input, i)

readLineArr :: IO [Int]
readLineArr = do
    putStr "Enter number: "
    inp <- readMaybe <$> getLine
    case inp of
        Nothing -> pure []
        Just x  -> pure [byteify x]



executeAll :: State -> Bool  -> IO State
executeAll (ti, td, output, input, i) slow = do
    case ti of
        Tape ls End rs -> pure (ti, td, output, input, i)
        _              -> do
                input' <- if null input then pure [] else readLineArr
                let s = executeInstruction (ti, td, output, input', i + 1)
                executeOnly slow $ do
                    threadDelay 10000
                    printState s

                executeAll s slow

executeOnly :: Bool -> IO () -> IO ()
executeOnly condition todo = if condition then todo else pure ()

executeAllInstructions :: String -> Bool -> IO (Maybe State)
executeAllInstructions instr slow = do
    case initState of
        Just s  -> Just <$> executeAll s slow
        Nothing -> return Nothing
    where
        initState :: Maybe State
        initState = (, pure 0, "", [], 0) <$> parseInstructions (lexInstructions instr)


replace :: String -> Char -> String -> String
replace str c r = foldr (\curr acc -> (if curr == c then r else [curr]) ++ acc) "" str

count :: String -> Char -> Int
count str c = length $ filter (==c) str

split :: Eq a => a -> [a] -> [[a]]
split x xs = (reverse <$>) <$> reverse $ split' x xs []
    where
        split' :: Eq a => a -> [a] -> [[a]] -> [[a]]
        split' x y [] = split' x y [[]]
        split' _ [] z = z
        split' x (y:ys) (a:as)
            | x == y = split' x ys ([]:(a:as))
            | otherwise = split' x ys ((y:a):as) -- queen?

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"

ctrlNewlines :: String -> [String]
ctrlNewlines str = (++"\ESC[B") . (\n -> (n++) $ concat $ replicate (length n) "\ESC[D") <$> split '\n' str

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

clearCLI :: IO ()
clearCLI = do
    putStr "\ESC[20A"
    putStr $ concat $ replicate 20 "\ESC[2K\ESC[B"

atLeast :: Int -> a -> [a] -> [a]
atLeast n x xs = xs ++ replicate (n - length xs) x

printState :: State  -> IO ()
printState (ti, td, output, _, i) = do
    putStr "\ESC[2J"
    putStrLn ""
    putStrLn $ "Output: " ++ reverse output
    putStrLn ""
    putStrLn $ "Instructions: " ++ tapeToPrettyString ti
    putStrLn ""
    putStrLn $ "Data: " ++ tapeToPrettyString td
    putStrLn ""
    putStrLn $ "Ran over " ++ red (show i) ++ " iterations"

    -- -- Fancier print, might be (is) broken
    -- clearCLI
    -- putStr $ "\ESC[12A" ++ concat (replicate 7 "\ESC[A")
    -- putStr $ "\ESC[2B\rOutput: " ++ concat (atLeast 7 "\ESC[B" $ lastN 7 $ ctrlNewlines (reverse output))
    -- putStr $ "\ESC[2B\rInstructions: " ++ showTape ti
    -- putStr $ "\ESC[2B\rData: " ++ showTape td
    -- putStr $ "\ESC[2B\rRan over " ++ red (show i) ++ " iterations"

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
                Nothing -> putStrLn "Invalid file."
