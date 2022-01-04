{-# LANGUAGE MultiWayIf #-}

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy
import           System.IO

type T = ReaderT Handle (StateT Integer IO)

symbol :: T Char
symbol = do
  f <- ask
  i <- lift get
  fileSize <- liftIO $ hFileSize f
  if i >= fileSize
    then return ' '
    else do
      liftIO $ hSeek f AbsoluteSeek i
      liftIO $ hGetChar f

p' :: Char -> T ()
p' c = do
  f <- ask
  i <- lift get
  liftIO $ hSeek f AbsoluteSeek i
  liftIO $ hPutChar f c

l' :: T ()
l' = do
  i <- lift get
  lift $ put (i - 1)

r' :: T ()
r' = do
  i <- lift get
  lift $ put (i + 1)

f :: T () -> T () -> Char -> T ()
f k r a = do
  s <- symbol
  if s == 'E'
    then do
      l'
      f1 k r a
    else do
      l'
      f k r a

f1 :: T () -> T () -> Char -> T ()
f1 k r a = do
  s <- symbol
  if | s == a -> k
     | s == ' ' ->
       do r'
          f2 k r a
     | otherwise ->
       do r'
          f1 k r a

f2 :: T () -> T () -> Char -> T ()
f2 k r a = do
  s <- symbol
  if | s == a -> k
     | s == ' ' ->
       do r'
          r
     | otherwise ->
       do r'
          f1 k r a

e :: T () -> T () -> Char -> T ()
e k r a = f (e1 k r a) r a

e1 :: T () -> T () -> Char -> T ()
e1 k r a = do
  p' ' '
  k

e' :: T () -> Char -> T ()
e' k a = e (e' k a) k a

pe :: T () -> Char -> T ()
pe k b = f (pe1 k b) k 'E'

pe1 :: T () -> Char -> T ()
pe1 k b = do
  s <- symbol
  if s == ' '
    then do
      p' b
      k
    else do
      r'
      symbol >>= p'
      r'
      pe1 k b

l :: T () -> T ()
l k = do
  l'
  k

f' :: T () -> T () -> Char -> T ()
f' k = f (l k)

c :: T () -> T () -> Char -> T ()
c k = f' (c1 k)

c1 :: T () -> T ()
c1 k = do
  b <- symbol
  pe k b

ce :: T () -> T () -> Char -> T ()
ce k r a = c (e k r a) r a

ce' :: T () -> Char -> T ()
ce' k a = ce (ce' k a) k a

re :: T () -> T () -> Char -> Char -> T ()
re k r a b = f (re1 k r a b) r a

re1 :: T () -> T () -> Char -> Char -> T ()
re1 k r a b = do
  p' ' '
  p' b
  k

re' :: T () -> Char -> Char -> T ()
re' k a b = re (re' k a b) k a b

cr :: T () -> T () -> Char -> T ()
cr k r a = c (re k r a 'a') r a

cr' :: T () -> Char -> T ()
cr' k a = cr (cr' k a) (re' k 'a' a) a

cp :: T () -> T () -> T () -> Char -> Char -> T ()
cp y n z a b = f' (cp1 y n b) (f n z b) a

cp1 :: T () -> T () -> Char -> T ()
cp1 y n b = do
  g <- symbol
  f' (cp2 y n g) n b

cp2 :: T () -> T () -> Char -> T ()
cp2 y n g = do
  s <- symbol
  if s == g
    then y
    else n

cpe :: T () -> T () -> T () -> Char -> Char -> T ()
cpe y n z a b = cp (e (e y y b) y a) n z a b

cpe' :: T () -> T () -> Char -> Char -> T ()
cpe' n y a b = cpe (cpe' n y a b) n y a b

g :: T () -> T ()
g k = do
  s <- symbol
  if s == ' '
    then do
      r'
      g1 k
    else do
      r'
      g k

g1 :: T () -> T ()
g1 k = do
  s <- symbol
  if s == ' '
    then k
    else do
      r'
      g k

g' :: T () -> Char -> T ()
g' k a = g (g1' k a)

g1' :: T () -> Char -> T ()
g1' k a = do
  s <- symbol
  if s == a
    then k
    else do
      l'
      g1' k a

pe2 :: T () -> Char -> Char -> T ()
pe2 k a b = pe (pe k b) a

ce2' :: T () -> Char -> Char -> T ()
ce2' k a b = ce' (ce' k b) a

ce3' :: T () -> Char -> Char -> Char -> T ()
ce3' k a b c = ce' (ce2' k b c) a

ce4' :: T () -> Char -> Char -> Char -> Char -> T ()
ce4' k a b c d = ce' (ce3' k b c d) a

ce5' :: T () -> Char -> Char -> Char -> Char -> Char -> T ()
ce5' k a b c d e = ce' (ce4' k b c d e) a

e2' :: T () -> Char -> Char -> T ()
e2' k a b = e' (e' k b) a

e'' :: T () -> T ()
e'' k = do
  s <- symbol
  if s == 'E'
    then do
      r'
      e1'' k
    else do
      l'
      e'' k

e1'' :: T () -> T ()
e1'' k = do
  s <- symbol
  if s == ' '
    then k
    else do
      r'
      p' ' '
      r'
      e1'' k

con :: T () -> Char -> T ()
con k a = do
  s <- symbol
  if s == 'A'
    then do
      l'
      p' a
      r'
      con1 k a
    else do
      r'
      r'
      con k a

con1 :: T () -> Char -> T ()
con1 k a = do
  s <- symbol
  if | s == 'D' ->
       do r'
          p' a
          r'
          con2 k a
     | s == 'A' ->
       do r'
          p' a
          r'
          con1 k a
     | otherwise -> liftIO $ print "unhandled"

con2 :: T () -> Char -> T ()
con2 k a = do
  s <- symbol
  if s /= 'C'
    then do
      r'
      r'
      k
    else do
      r'
      p' a
      r'
      con2 k a

{- THE UNIVERSAL MACHINE -}
main = do
  f <- openFile "tape" ReadWriteMode
  runStateT (runReaderT b f) 2
  hClose f

b = f b1 b1 '.'

b1 = do
  r'
  p' ' '
  r'
  p' ':'
  r'
  p' ' '
  r'
  p' 'D'
  r'
  p' ' '
  r'
  p' 'A'
  r'
  p' ' '
  r'
  p' 'D'
  anf

anf = g' anf1 ':'

anf1 = con fom 'y'

fom = do
  s <- symbol
  if | s == ';' ->
       do r'
          p' 'z'
          l'
          con fmp 'x'
     | s == 'z' ->
       do l'
          l'
          fom
     | otherwise ->
       do l'
          fom

fmp = cpe' (e2' anf 'x' 'y') sim 'x' 'y'

sim = f' sim1 sim1 'z'

sim1 = con sim2 ' '

sim2 = do
  s <- symbol
  if s == 'A'
    then sim3
    else do
      l'
      p' 'u'
      r'
      r'
      r'
      sim2

sim3 = do
  s <- symbol
  if s /= 'A'
    then do
      l'
      p' 'y'
      e' mf 'z'
    else do
      l'
      p' 'y'
      r'
      r'
      r'
      sim3

mf = g' mf1 ':'

mf1 = do
  s <- symbol
  if s == 'A'
    then do
      l'
      l'
      l'
      l'
      mf2
    else do
      r'
      r'
      mf1

mf2 = do
  s <- symbol
  if | s == ':' -> mf4
     | s == 'C' ->
       do r'
          p' 'x'
          l'
          l'
          l'
          mf2
     | s == 'D' ->
       do r'
          p' 'x'
          l'
          l'
          l'
          mf3
     | otherwise -> liftIO $ print "unhandled"

mf3 = do
  s <- symbol
  if s == ':'
    then mf4
    else do
      r'
      p' 'v'
      l'
      l'
      l'
      mf3

mf4 = con (l (l mf5)) ' '

mf5 = do
  s <- symbol
  if s == ' '
    then do
      p' ':'
      sh
    else do
      r'
      p' 'w'
      r'
      mf5

sh = f sh1 inst 'u'

sh1 = do
  l'
  sh2

sh2 = do
  s <- symbol
  if s == 'D'
    then do
      r'
      r'
      sh3
    else inst

sh3 = do
  s <- symbol
  if s /= 'C'
    then inst
    else do
      r'
      r'
      sh4

sh4 = do
  s <- symbol
  if s /= 'C'
    then pe2 inst '0' ':'
    else do
      r'
      r'
      sh5

sh5 = do
  s <- symbol
  if s /= 'C'
    then pe2 inst '1' ':'
    else inst

inst = g' (l inst1) 'u'

inst1 = do
  s <- symbol
  r'
  p' ' '
  case s of
    'L' -> ce5' ov 'v' 'y' 'x' 'u' 'w'
    'R' -> ce5' ov 'v' 'x' 'u' 'y' 'w'
    _   -> ce5' ov 'v' 'x' 'y' 'u' 'w'

ov = e'' (pe anf 'D')
