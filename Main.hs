import Data.List
import Data.Char
import System.IO
import System.Environment

data Zipper a = Zip ![a] ![a]

instance Show a => Show (Zipper a) where
    show (Zip [] []) = "[]"
    show (Zip (x:xs) r) = concat 
        [ "["
        , intercalate "," (map show $ reverse xs)
        , (if null xs then "" else ",") ++ "{" ++ show x ++ "}" ++ (if null r then "" else ",")
        , intercalate "," (map show r)
        , "]"
        ]

fromList :: [a] -> Zipper a
fromList [] = Zip [] []
fromList (h:t) = Zip [h] t

leftWithDefault :: a -> Zipper a -> Zipper a
leftWithDefault x (Zip (h:h':t) r) = Zip (h':t) (h:r)
leftWithDefault x (Zip l r) = Zip [x] (l++r)

rightWithDefault :: a -> Zipper a -> Zipper a
rightWithDefault x (Zip l (h:t)) = Zip (h:l) t
rightWithDefault x (Zip l []) = Zip (x:l) []

unsafeMapCursor :: (a -> a) -> Zipper a -> Zipper a
unsafeMapCursor f (Zip (x:xs) r) = Zip (f x:xs) r
unsafeMapCursor _ _ = error "unsafeMapCursor: empty zipper"

unsafeCursor :: Zipper a -> a
unsafeCursor (Zip (x:xs) _) = x
unsafeCursor _ = error "unsafeCursor: empty zipper"

data Inst = 
    InstLeft 
    | InstRight 
    | InstPlus 
    | InstMinus 
    | InstOut 
    | InstIn 
    | InstBWhile
    | InstEWhile
    | InstNop
    | InstHalt 
    deriving Eq

instance Show Inst where
    show InstLeft   = "<"
    show InstRight  = ">"
    show InstPlus   = "+"
    show InstMinus  = "-"
    show InstOut    = "."
    show InstIn     = ","
    show InstBWhile = "["
    show InstEWhile = "]"
    show InstNop    = "N"
    show InstHalt   = "H"

chrToInst :: Char -> Inst
chrToInst '<' = InstLeft
chrToInst '>' = InstRight
chrToInst '-' = InstMinus
chrToInst '+' = InstPlus
chrToInst '.' = InstOut
chrToInst ',' = InstIn
chrToInst '[' = InstBWhile
chrToInst ']' = InstEWhile
chrToInst _   = InstNop

data State = State 
    { cells :: Zipper Integer
    , insts :: Zipper Inst
    }

instance Show State where
    show st = unlines
        [ "State"
        , "  cells  : " ++ show (cells st)
        , "  insts  : " ++ show (insts st)
        ]

initial :: [Inst] -> State
initial insts = State
    { cells = fromList [0]
    , insts = fromList $ filter (/=InstNop) insts
    }

stateDefault = initial [InstLeft, InstRight]

fowInst st = st{insts = rightWithDefault InstHalt $ insts st}
rewInst st = st{insts = leftWithDefault  InstHalt $ insts st}

runInst' :: Inst -> State -> IO State
runInst' inst st = case inst of
    InstLeft ->
        return st{cells = leftWithDefault 0 (cells st)}
    InstRight ->
        return st{cells = rightWithDefault 0 (cells st)}
    InstPlus ->
        return st{cells = unsafeMapCursor (\x -> x + 1) (cells st)}
    InstMinus ->
        return st{cells = unsafeMapCursor (\x -> x - 1) (cells st)}
    InstOut -> do
        let c = chr . fromIntegral . unsafeCursor . cells $ st
        hPutChar stdout c
        return st
    InstIn -> do
        x <- fromIntegral <$> ord <$> hGetChar stdin
        return st{cells = unsafeMapCursor (const x) (cells st)}
    InstBWhile -> do
        return $ case unsafeCursor (cells st) of
            0 -> rewInst $ bwhile fowInst 1 (fowInst st)            
            _ -> st
    InstEWhile -> do
        return $ case unsafeCursor (cells st) of
            0 -> st
            _ -> bwhile rewInst (-1) (rewInst st)            
    InstNop ->
        return st
    InstHalt ->
        return st
    where
        bwhile :: (State -> State) -> Int -> State -> State
        bwhile skip 0 st = st
        bwhile skip n st = case unsafeCursor (insts st) of
            InstHalt -> st
            InstBWhile -> bwhile skip (n+1) (skip st)
            InstEWhile -> bwhile skip (n-1) (skip st)
            _ -> bwhile skip n (skip st)

runInst :: State -> IO (Inst, State)
runInst st =
    let inst = unsafeCursor $ insts st 
    in do
        st' <- runInst' inst st
        return (inst, fowInst st')

runInsts :: State -> IO State
runInsts st = do 
    ist <- runInst st
    case ist of
        (InstHalt, st) ->
            return st
        (_, st) ->
            runInsts st

loadState :: FilePath -> IO State
loadState path = do
    initial <$> map chrToInst <$> readFile path

main = do
    pname <- getProgName
    args <- getArgs
    case args of 
        [] ->
            putStrLn ("Usage: " ++ pname ++ " [FILE]")
        [fname] -> do
            state <- loadState fname
            runInsts state
            return ()
