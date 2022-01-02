{-# LANGUAGE MultiWayIf #-}

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy
import           System.IO

type Table = ReaderT Handle (StateT Integer IO)

moveLeft :: Table ()
moveLeft = do
  i <- lift get
  lift $ put (i - 1)

moveRight :: Table ()
moveRight = do
  i <- lift get
  lift $ put (i + 1)

readSymbol :: Table Char
readSymbol = do
  f <- ask
  i <- lift get
  fileSize <- liftIO $ hFileSize f
  if i >= fileSize
    then return ' '
    else do
      liftIO $ hSeek f AbsoluteSeek i
      liftIO $ hGetChar f

printSymbol :: Char -> Table ()
printSymbol c = do
  f <- ask
  i <- lift get
  liftIO $ hSeek f AbsoluteSeek i
  liftIO $ hPutChar f c

f :: Table () -> Table () -> Char -> Table ()
f k r a = do
  s <- readSymbol
  if s == 'E'
    then do
      moveLeft
      f1 k r a
    else do
      moveLeft
      f k r a

f1 :: Table () -> Table () -> Char -> Table ()
f1 k r a = do
  s <- readSymbol
  if | s == a -> k
     | s == ' ' ->
       do moveRight
          f2 k r a
     | otherwise ->
       do moveRight
          f1 k r a

f2 :: Table () -> Table () -> Char -> Table ()
f2 k r a = do
  s <- readSymbol
  if | s == a -> k
     | s == ' ' ->
       do moveRight
          r
     | otherwise ->
       do moveRight
          f1 k r a

e :: Table () -> Table () -> Char -> Table ()
e k r a = f (e1 k r a) r a

e1 :: Table () -> Table () -> Char -> Table ()
e1 k r a = do
  printSymbol ' '
  k

e' :: Table () -> Char -> Table ()
e' k a = e (e' k a) k a

pe :: Table () -> Char -> Table ()
pe k b = f (pe1 k b) k 'E'

pe1 :: Table () -> Char -> Table ()
pe1 k b = do
  s <- readSymbol
  if s == ' '
    then do
      printSymbol b
      k
    else do
      moveRight
      readSymbol >>= printSymbol
      moveRight
      pe1 k b

l :: Table () -> Table ()
l k = do
  moveLeft
  k

f' :: Table () -> Table () -> Char -> Table ()
f' k = f (l k)

c :: Table () -> Table () -> Char -> Table ()
c k = f' (c1 k)

c1 :: Table () -> Table ()
c1 k = do
  b <- readSymbol
  pe k b

ce :: Table () -> Table () -> Char -> Table ()
ce k r a = c (e k r a) r a

ce' :: Table () -> Char -> Table ()
ce' k a = ce (ce' k a) k a

re :: Table () -> Table () -> Char -> Char -> Table ()
re k r a b = f (re1 k r a b) r a

re1 :: Table () -> Table () -> Char -> Char -> Table ()
re1 k r a b = do
  printSymbol ' '
  printSymbol b
  k

re' :: Table () -> Char -> Char -> Table ()
re' k a b = re (re' k a b) k a b

cr :: Table () -> Table () -> Char -> Table ()
cr k r a = c (re k r a 'a') r a

cr' :: Table () -> Char -> Table ()
cr' k a = cr (cr' k a) (re' k 'a' a) a

cp :: Table () -> Table () -> Table () -> Char -> Char -> Table ()
cp y n z a b = f' (cp1 y n b) (f n z b) a

cp1 :: Table () -> Table () -> Char -> Table ()
cp1 y n b = do
  g <- readSymbol
  f' (cp2 y n g) n b

cp2 :: Table () -> Table () -> Char -> Table ()
cp2 y n g = do
  s <- readSymbol
  if s == g
    then y
    else n

cpe :: Table () -> Table () -> Table () -> Char -> Char -> Table ()
cpe y n z a b = cp (e (e y y b) y a) n z a b

cpe' :: Table () -> Table () -> Char -> Char -> Table ()
cpe' n y a b = cpe (cpe' n y a b) n y a b

g :: Table () -> Table ()
g k = do
  s <- readSymbol
  if s == ' '
    then do
      moveRight
      g1 k
    else do
      moveRight
      g k

g1 :: Table () -> Table ()
g1 k = do
  s <- readSymbol
  if s == ' '
    then k
    else do
      moveRight
      g k

g' :: Table () -> Char -> Table ()
g' k a = g (g1' k a)

g1' :: Table () -> Char -> Table ()
g1' k a = do
  s <- readSymbol
  if s == a
    then k
    else do
      moveLeft
      g1' k a

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
e'' k = do
  s <- readSymbol
  if s == 'E'
    then do
      moveRight
      e1'' k
    else do
      moveLeft
      e'' k

e1'' :: Table () -> Table ()
e1'' k = do
  s <- readSymbol
  if s == ' '
    then k
    else do
      moveRight
      printSymbol ' '
      moveRight
      e1'' k

con :: Table () -> Char -> Table ()
con k a = do
  s <- readSymbol
  if s == 'A'
    then do
      moveLeft
      printSymbol a
      moveRight
      con1 k a
    else do
      moveRight
      moveRight
      con k a

con1 :: Table () -> Char -> Table ()
con1 k a = do
  s <- readSymbol
  if | s == 'D' ->
       do moveRight
          printSymbol a
          moveRight
          con2 k a
     | s == 'A' ->
       do moveRight
          printSymbol a
          moveRight
          con1 k a
     | otherwise -> liftIO $ print "unhandled"

con2 :: Table () -> Char -> Table ()
con2 k a = do
  s <- readSymbol
  if s /= 'C'
    then do
      moveRight
      moveRight
      k
    else do
      moveRight
      printSymbol a
      moveRight
      con2 k a

{- THE UNIVERSAL MACHINE -}
main = do
  f <- openFile "tape" ReadWriteMode
  runStateT (runReaderT b f) 2
  hClose f

b = f b1 b1 '.'

b1 = do
  moveRight
  printSymbol ' '
  moveRight
  printSymbol ':'
  moveRight
  printSymbol ' '
  moveRight
  printSymbol 'D'
  moveRight
  printSymbol ' '
  moveRight
  printSymbol 'A'
  moveRight
  printSymbol ' '
  moveRight
  printSymbol 'D'
  anf

anf = g' anf1 ':'

anf1 = con fom 'y'

fom = do
  s <- readSymbol
  if | s == ';' ->
       do moveRight
          printSymbol 'z'
          moveLeft
          con fmp 'x'
     | s == 'z' ->
       do moveLeft
          moveLeft
          fom
     | otherwise ->
       do moveLeft
          fom

fmp = cpe' (e2' anf 'x' 'y') sim 'x' 'y'

sim = f' sim1 sim1 'z'

sim1 = con sim2 ' '

sim2 = do
  s <- readSymbol
  if s == 'A'
    then sim3
    else do
      moveLeft
      printSymbol 'u'
      moveRight
      moveRight
      moveRight
      sim2

sim3 = do
  s <- readSymbol
  if s /= 'A'
    then do
      moveLeft
      printSymbol 'y'
      e' mf 'z'
    else do
      moveLeft
      printSymbol 'y'
      moveRight
      moveRight
      moveRight
      sim3

mf = g' mf1 ':'

mf1 = do
  s <- readSymbol
  if s == 'A'
    then do
      moveLeft
      moveLeft
      moveLeft
      moveLeft
      mf2
    else do
      moveRight
      moveRight
      mf1

mf2 = do
  s <- readSymbol
  if | s == ':' -> mf4
     | s == 'C' ->
       do moveRight
          printSymbol 'x'
          moveLeft
          moveLeft
          moveLeft
          mf2
     | s == 'D' ->
       do moveRight
          printSymbol 'x'
          moveLeft
          moveLeft
          moveLeft
          mf3
     | otherwise -> liftIO $ print "unhandled"

mf3 = do
  s <- readSymbol
  if s == ':'
    then mf4
    else do
      moveRight
      printSymbol 'v'
      moveLeft
      moveLeft
      moveLeft
      mf3

mf4 = con (l (l mf5)) ' '

mf5 = do
  s <- readSymbol
  if s == ' '
    then do
      printSymbol ':'
      sh
    else do
      moveRight
      printSymbol 'w'
      moveRight
      mf5

sh = f sh1 inst 'u'

sh1 = do
  moveLeft
  sh2

sh2 = do
  s <- readSymbol
  if s == 'D'
    then do
      moveRight
      moveRight
      sh3
    else inst

sh3 = do
  s <- readSymbol
  if s /= 'C'
    then inst
    else do
      moveRight
      moveRight
      sh4

sh4 = do
  s <- readSymbol
  if s /= 'C'
    then pe2 inst '0' ':'
    else do
      moveRight
      moveRight
      sh5

sh5 = do
  s <- readSymbol
  if s /= 'C'
    then pe2 inst '1' ':'
    else inst

inst = g' (l inst1) 'u'

inst1 = do
    s <- readSymbol
    moveRight
    printSymbol ' '
    case s of
      'L' -> ce5' ov 'v' 'y' 'x' 'u' 'w'
      'R' -> ce5' ov 'v' 'x' 'u' 'y' 'w'
      _ -> ce5' ov 'v' 'x' 'y' 'u' 'w'

ov = e'' (pe anf 'D')
