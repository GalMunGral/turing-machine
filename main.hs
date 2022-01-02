{-# LANGUAGE MultiWayIf #-}

import System.IO
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

type Table = ReaderT Handle (StateT Integer IO)

_L :: Table ()
_L = do
    i <- lift get
    lift $ put (i - 1)

_R :: Table ()
_R = do
    i <- lift get
    lift $ put (i + 1)

_S :: Table Char
_S = do
    f <- ask
    i <- lift get
    fileSize <- liftIO $ hFileSize f
    if i >= fileSize then return ' ' else do
        liftIO $ hSeek f AbsoluteSeek i
        liftIO $ hGetChar f

_P :: Char -> Table ()
_P c = do
    f <- ask
    i <- lift get
    liftIO $ hSeek f AbsoluteSeek i
    liftIO $ hPutChar f c

f :: Table () -> Table () -> Char -> Table ()
f k r a = _S >>= \s -> if 
    | s == 'E'  -> do _L; f1 k r a
    | otherwise -> do _L; f k r a

f1 :: Table () -> Table () -> Char -> Table ()
f1 k r a = _S >>= \s -> if
    | s == a    -> k
    | s == ' '  -> do _R; f2 k r a
    | otherwise -> do _R; f1 k r a

f2 :: Table () -> Table () -> Char -> Table ()
f2 k r a = _S >>= \s -> if
    | s == a    ->  k
    | s == ' '  -> do _R; r
    | otherwise -> do _R; f1 k r a

e :: Table () -> Table () -> Char -> Table ()
e k r a = f (e1 k r a) r a

e1 :: Table () -> Table () -> Char -> Table ()
e1 k r a = do _P ' '; k

e' :: Table () -> Char -> Table ()
e' k a = e (e' k a) k a

pe :: Table () -> Char -> Table ()
pe k b = f (pe1 k b) k 'E'

pe1 :: Table () -> Char -> Table ()
pe1 k b = _S >>= \s -> if
    | s == ' '  -> do _P b; k
    | otherwise -> do _R; (_S >>= _P); _R; pe1 k b

l :: Table () -> Table ()
l k = do _L; k

f' :: Table () -> Table () -> Char -> Table ()
f' k r a = f (l k) r a

c :: Table () -> Table () -> Char -> Table ()
c k r a = f' (c1 k) r a

c1 :: Table () -> Table ()
c1 k = _S >>= \b -> pe k b

ce :: Table () -> Table () -> Char -> Table ()
ce k r a = c (e k r a) r a

ce' :: Table () -> Char -> Table ()
ce' k a = ce (ce' k a) k a

re :: Table() -> Table () -> Char -> Char -> Table ()
re k r a b = f (re1 k r a b) r a

re1 :: Table() -> Table () -> Char -> Char -> Table ()
re1 k r a b = do _P ' '; _P b; k

re' :: Table () -> Char -> Char -> Table ()
re' k a b = re (re' k a b) k a b

cr :: Table () -> Table () -> Char -> Table ()
cr k r a = c (re k r a 'a') r a

cr' :: Table () -> Char -> Table ()
cr' k a = cr (cr' k a) (re' k 'a' a) a

cp :: Table () -> Table () -> Table () -> Char -> Char -> Table ()
cp y n z a b = f' (cp1 y n b) (f n z b) a

cp1 :: Table () -> Table () -> Char -> Table ()
cp1 y n b = _S >>= \g -> f' (cp2 y n g) n b

cp2 :: Table () -> Table () -> Char -> Table ()
cp2 y n g = _S >>= \s -> if
    | s == g    -> y
    | otherwise -> n

cpe :: Table () -> Table () -> Table () -> Char -> Char -> Table ()
cpe y n z a b = cp (e (e y y b) y a) n z a b

cpe' :: Table() -> Table () -> Char -> Char -> Table () 
cpe' n y a b = cpe (cpe' n y a b) n y a b

g :: Table () -> Table () 
g k = _S >>= \s -> if
    | s == ' '  -> do _R; g1 k
    | otherwise -> do _R; g k

g1 :: Table () -> Table () 
g1 k = _S >>= \s -> if
    | s == ' '  -> k
    | otherwise -> do _R; g k

g' :: Table () -> Char -> Table ()
g' k a = g (g1' k a)

g1' :: Table () -> Char -> Table ()
g1' k a = _S >>= \s -> if
    | s == a    -> k
    | otherwise -> do _L; g1' k a

pe2 :: Table () -> Char -> Char -> Table ()
pe2 k a b = pe (pe k b) a

ce2' :: Table () -> Char -> Char -> Table ()
ce2' k a b = ce' (ce' k b) a

ce3' :: Table () -> Char -> Char -> Char -> Table ()
ce3' k a b c = ce' (ce2' k b c) a

ce4' :: Table () -> Char -> Char -> Char -> Char -> Table ()
ce4' k a b c d = ce' (ce3' k b c d) a

ce5' :: Table () -> Char -> Char -> Char -> Char -> Char -> Table ()
ce5' k a b c d e = ce' (ce4' k b c d e) a

e2' :: Table () -> Char -> Char -> Table ()
e2' k a b = e' (e' k b) a

e'' :: Table () -> Table ()
e'' k = _S >>= \s -> if
    | s == 'E'  -> do _R; e1'' k
    | otherwise -> do _L; e'' k

e1'' :: Table () -> Table ()
e1'' k = _S >>= \s -> if
    | s == ' '  -> k
    | otherwise -> do _R; _P ' '; _R; e1'' k

con :: Table () -> Char -> Table ()
con k a = _S >>= \s -> if
    | s == 'A'  -> do _L; _P a; _R; con1 k a
    | otherwise -> do _R; _R; con k a

con1 :: Table () -> Char -> Table ()
con1 k a = _S >>= \s -> if
    | s == 'D'  -> do _R; _P a; _R; con2 k a;
    | s == 'A'  -> do _R; _P a; _R; con1 k a;

con2 :: Table () -> Char -> Table ()
con2 k a = _S >>= \s -> if
    | s /= 'C'  -> do _R; _R; k
    | otherwise -> do _R; _P a; _R; con2 k a
    
{- THE UNIVERSAL MACHINE -}

main = do
    f <- openFile "tape" ReadWriteMode
    runStateT (runReaderT b f) 2
    hClose f

b = f b1 b1 '.'
b1 = do
    _R; _P ' '; _R; _P ':'
    _R; _P ' '; _R; _P 'D'
    _R; _P ' '; _R; _P 'A'
    _R; _P ' '; _R; _P 'D'
    anf

anf = g' anf1 ':'
anf1 = con fom 'y'

fom = _S >>= \s -> if
    | s == ';'  -> do _R; _P 'z'; _L; con fmp 'x'
    | s == 'z'  -> do _L; _L; fom
    | otherwise -> do _L; fom
fmp = cpe' (e2' anf 'x' 'y') sim 'x' 'y'

sim = f' sim1 sim1 'z'
sim1 = con sim2 ' '
sim2 = _S >>= \s -> if
    | s == 'A'  -> sim3
    | otherwise -> do _L; _P 'u'; _R; _R; _R; sim2
sim3 = _S >>= \s -> if
    | s /= 'A' -> do _L; _P 'y'; e' mf 'z'
    | otherwise -> do _L; _P 'y'; _R; _R; _R; sim3

mf = g' mf1 ':'
mf1 = _S >>= \s -> if
    | s == 'A'  -> do _L; _L; _L; _L; mf2
    | otherwise -> do _R; _R; mf1
mf2 = _S >>= \s -> if
    | s == ':'  -> mf4
    | s == 'C'  -> do _R; _P 'x'; _L; _L; _L; mf2
    | s == 'D'  -> do _R; _P 'x'; _L; _L; _L; mf3
mf3 = _S >>= \s -> if
    | s == ':'  -> mf4
    | otherwise -> do _R; _P 'v'; _L; _L; _L; mf3
mf4 = con (l (l mf5)) ' '
mf5 = _S >>= \s -> if
    | s == ' '  -> do _P ':'; sh
    | otherwise -> do _R; _P 'w'; _R; mf5

sh = f sh1 inst 'u'
sh1 = do _L; sh2
sh2 = _S >>= \s -> if
    | s == 'D'  -> do _R; _R; sh3
    | otherwise -> inst
sh3 = _S >>= \s -> if
    | s /= 'C'  -> inst
    | otherwise -> do _R; _R; sh4
sh4 = _S >>= \s -> if
    | s /= 'C'  -> pe2 inst '0' ':'
    | otherwise -> do _R; _R; sh5
sh5 = _S >>= \s -> if
    | s /= 'C' -> pe2 inst '1' ':'
    | otherwise -> inst

inst = g' (l inst1) 'u'
inst1 = _S >>= \s -> do
    _R; _P ' '
    case s of
        'L' -> (ce5' ov 'v' 'y' 'x' 'u' 'w')
        'R' -> (ce5' ov 'v' 'x' 'u' 'y' 'w')
        'N' -> (ce5' ov 'v' 'x' 'y' 'u' 'w')

ov = e'' (pe anf 'D')
