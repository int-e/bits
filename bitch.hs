-- a "bitch" interpreter
--
-- original:
--   https://github.com/Helen0903/bitch
--
-- language description:
--   https://esolangs.org/wiki/bitch
--
-- to compile:
--   ghc --make bitch.hs
--
-- to use:
--   ./bitch program_file
--
-- differences to github implementation:
-- - use big integers
-- - additional operation % dumps the current state and the next instruction
--   format:  % .. accumulator (in hex) | reverse storage .. instruction
--   example: % .. 0000 0000 0000 0001 | 8000 0000 0000 0000 .. ^^[1
-- - (fix) input in chained instruction reads from same line as unchained
--   instructions

import Control.Monad
import Data.Bits
import Numeric
import System.Environment
import System.Exit
import Text.Read (readMaybe)

maxShift :: Integer
maxShift = 2^24

-- basic operations: flow, I/O, bitwise (unary), extra
data Op0 = MARK | RESET | STOP | READ | WRITE | LIT Integer | NOT | DUMP

-- bitwise operations (binary), clear storage modifier
data Op1 = AND | OR | XOR | SHL | SHR | CLS

-- conditionals
data Cmp = EQZ | NEZ

data Op = COND Cmp Op | Op0 Op0 | Op1 Op1 Op

type Program = [Op]

-- program state
data World = World {
    accum :: Integer,
    store :: Integer,
    reset :: Program,
    input :: String
}

world0 :: String -> Program -> World
world0 input prog = World {
    accum = 0,
    store = 0,
    reset = prog,
    input = input
 }

data Config = Config {
    charIO :: Bool
}

-- execute a program
exec :: Config -> Program -> World -> IO ()
exec cfg (o : os) w = do
    (w', os') <- op o
    exec cfg os' w'
  where
    -- conditional
    op (COND EQZ o) = if accum w == 0 then op o else return (w, os)
    op (COND NEZ o) = if accum w /= 0 then op o else return (w, os)
    -- basic operation
    op (Op0 NOT) = return (w{ accum = complement (accum w) }, os)
    op (Op0 STOP) = return (w, [])
    op (Op0 MARK) = return (w{ reset = os }, os)
    op (Op0 RESET) = return (w, reset w)
    op (Op0 READ) = case (charIO cfg, input w, reads (input w)) of
       (True, x : xs, _) ->
           let n = fromIntegral (fromEnum x) in
           return (w{ accum = n, store = 0, input = xs }, os)
       (False, _, [(n, xs)]) ->
           return (w{ accum = n, store = 0, input = xs }, os)
       (_, _, _) ->
           return (w{ accum = -1, store = 0 }, os)
    op (Op0 WRITE) = do
        if charIO cfg then putChar (toEnum (fromIntegral (accum w)))
                      else print (accum w)
        return (w, os)
    op (Op0 (LIT n)) = return (w{ accum = n }, os)
    op (Op0 DUMP) = do
        putStrLn $ unwords ["%", pWorld w, take 1 os >>= pOp]
        return (w, os)
    -- bitwise operation (binary)
    op (Op1 o1 o) = do
        (w', _) <- op o
        return (op1 o1 (accum w') w{ input = input w' }, os)

    op1 o1 val w = case o1 of
        AND -> w{ accum = accum w .&. val }
        OR  -> w{ accum = accum w .|. val }
        XOR -> w{ accum = accum w `xor` val }
        SHL -> let (a, s) = shlOp (accum w, store w) val in
               w{ accum = a, store = s }
        SHR -> let (s, a) = shlOp (store w, accum w) val in
               w{ accum = a, store = s }
        CLS -> w{ accum = val, store = 0 }

    -- shift lower s bits of b into a
    shlOp (a, b) s
        | s < 0 = (a, b)
        | s > maxShift = error $ "shifting by more than " ++
            show maxShift ++ " bits at once, giving up!"
        | otherwise = (a `shiftL` s' .|. bBits, b `shiftR` s')
      where
        s' = fromIntegral s
        bBits = sum [bit i | i <- [0..s'-1], testBit b (s' - 1 - i)]
exec _ _ _ = return ()

main :: IO ()
main = do
    as <- getArgs
    (cfg, f) <- case as of
        ["-c", f] -> return (Config{ charIO = True }, f)
        [f] -> return (Config{ charIO = False }, f)
        _ -> putStrLn "Usage: $0 [-c] file" >> exitFailure
    prog <- parseProg <$> readFile f
    input <- getContents
    void $ exec cfg prog (world0 input prog)

-- parsing
parseProg :: String -> [Op]
parseProg xs = case xs of
    '~' : xs -> Op0 NOT : parseProg xs
    '.' : xs -> Op0 STOP : parseProg xs
    '>' : xs -> Op0 MARK : parseProg xs
    '<' : xs -> Op0 RESET : parseProg xs
    '\\' : xs -> Op0 READ : parseProg xs
    '/' : xs -> Op0 WRITE : parseProg xs
    '%' : xs -> Op0 DUMP : parseProg xs
    '&' : xs -> op1 AND xs
    '|' : xs -> op1 OR xs
    '^' : xs -> op1 XOR xs
    ']' : xs -> op1 SHR xs
    '[' : xs -> op1 SHL xs
    '#' : xs -> op1 CLS xs
    ':' : xs -> cond EQZ (parseProg xs)
    ';' : xs -> cond NEZ (parseProg xs)
    _ -> []
  where
    numChars = "-0123456789"

    op1 op1 xs@(x : _) | x `elem` numChars =
        let (n, xs') = span (`elem` numChars) xs in
        case readMaybe n of
            Just n -> Op1 op1 (Op0 (LIT n)) : parseProg xs'
            _ -> []
    op1 op1 xs = case parseProg xs of
        op : os -> Op1 op1 op : os
        _  -> []

    cond cmp (o : os) = COND cmp o : os
    cond _ _ = []

-- pretty printing
pCmp :: Cmp -> Char
pCmp EQZ = ':'
pCmp NEZ = ';'

pOp0 :: Op0 -> String
pOp0 NOT = "~"
pOp0 STOP = "."
pOp0 MARK = ">"
pOp0 RESET = "<"
pOp0 READ = "\\"
pOp0 WRITE = "/"
pOp0 (LIT n) = show n
pOp0 DUMP = "%"

pOp1 :: Op1 -> Char
pOp1 AND = '&'
pOp1 OR = '|'
pOp1 XOR = '^'
pOp1 SHL = '['
pOp1 SHR = ']'
pOp1 CLS = '#'

pOp :: Op -> String
pOp (COND cmp op) = pCmp cmp : pOp op
pOp (Op0 op0) = pOp0 op0
pOp (Op1 op1 op) = pOp1 op1 : pOp op

pWorld :: World -> String
pWorld World{ accum = a, store = s } =
    unwords ["..", hex a, "|", hex (rev s), ".."]
  where
    g = 4
    w = 6*g
    b = 1 `shiftL` (4 * w)
    hex a = grp $ tail (showHex (b .|. a .&. (b-1)) "")
    rev s = sum [bit i | i <- [0..4*w-1], testBit s (4*w - 1 - i)]
    grp = unwords . map (take g) . takeWhile (not . null) . iterate (drop g)
