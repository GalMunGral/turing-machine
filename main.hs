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

m = do
    ce' (return ()) 'x'

main = do
    f <- openFile "tape" ReadWriteMode
    runStateT (runReaderT m f) 2
    hClose f
{-


;;;; THE UNIVERSAL MACHINE

(defun b/begin ()
  (f/find-first ^(b1/init) ^(b1/init) #\.))

(defun b1/init ()
  (R) (P #\Space) (R) (P #\:)
  (R) (P #\Space) (R) (P #\D)
  (R) (P #\Space) (R) (P #\A)
  (R) (P #\Space) (R) (P #\D)
  (anf/advance-to-next))

(defun anf/advance-to-next ()
  (g/find-last ^(anf1/advance-to-next) #\:))

(defun anf1/advance-to-next ()
  (con/mark-config ^(fom/find-matching-rule) #\y))

(defun fom/find-matching-rule ()
  (cond
    ((eq (cur-sym) #\;)
      (R) (P #\z) (L) ; mark visited
      (con/mark-config ^(fmp/match-rule) #\x)) 
    ((eq (cur-sym) #\z)
      (L) (L) ; skip visited rules
      (fom/find-matching-rule))
    (t
      (L)
      (fom/find-matching-rule))))

(defun fmp/match-rule ()
  ;; There seems to be a mistake here:
  ;; On error the machine should jump to 'anf' state
  ;; rather than 'fom' as 'y' marks have been erased
  (cpe/compare-sequence-erase ^(e/erase-all-2 ^(anf/advance-to-next) #\x #\y)
    ^(sim/simulate-rule) #\x #\y))

(defun sim/simulate-rule ()
  (f/find-first-marked ^(sim1/simulate) ^(sim1/simulate) #\z))

(defun sim1/simulate ()
  (con/mark-config ^(sim2/mark-operation) #\Space))

(defun sim2/mark-operation ()
  (cond
    ((eq (cur-sym) #\A)
      (sim3/mark-next-state))
    (t
      ;; The paper says RPRRR, seems to be a mistake ??
      (L) (P #\u) (R)
      (R) (R)
      (sim2/mark-operation))))

(defun sim3/mark-next-state ()
  (cond
    ((not (eq (cur-sym) #\A))
      (L) (P #\y)
      (e/erase-all ^(mf/mark-full-config) #\z))
    (t
      (L) (P #\y) (R)
      (R) (R)
      (sim3/mark-next-state))))

(defun mf/mark-full-config ()
  ;; typo - 'mf' should be 'mf1'
  (g/find-last ^(mf1/find-cursor) #\:))

(defun mf1/find-cursor ()
  (cond
    ((eq (cur-sym) #\A)
      (L) (L) (L) (L)
      (mf2/mark-prev-symbols))
    (t
      (R) (R)
      (mf1/find-cursor))))

(defun mf2/mark-prev-symbols ()
  (cond
    ((eq (cur-sym) #\:)
      (mf4/mark-next-symbols))
    ((eq (cur-sym) #\C)
      (R) (P #\x) (L)
      (L) (L)
      (mf2/mark-prev-symbols))
    ((eq (cur-sym) #\D)
      (R) (P #\x) (L)
      (L) (L)
      (mf3/mark-prev-symbols))))

(defun mf3/mark-prev-symbols ()
  (cond
    ((eq (cur-sym) #\:)
      (mf4/mark-next-symbols))
    (t
      (R) (P #\v) (L)
      (L) (L)
      (mf3/mark-prev-symbols))))

(defun mf4/mark-next-symbols ()
  (con/mark-config ^(l/move-left ^(l/move-left ^(mf5/mark-next-symbols))) #\Space))

(defun mf5/mark-next-symbols ()
  (cond
    ((none (cur-sym))
      (P #\:)
      (sh/print-output))
    (t
      (R) (P #\w) (R)
      (mf5/mark-next-symbols))))

(defun sh/print-output ()
  (f/find-first ^(sh1/print-output) ^(inst/print-full-config) #\u))

(defun sh1/print-output ()
  (L) ; LLL in the paper ??
  (sh2/print-output))

(defun sh2/print-output ()
  (cond
    ((eq (cur-sym) #\D)
      (R) (R) ; RRRR in the paper ??
      (sh3/print-output)) ; typo: 'sh2' should be 'sh3'
    (t
      (inst/print-full-config))))

(defun sh3/print-output ()
  (cond
    ((not (eq (cur-sym) #\C))
      (inst/print-full-config))
    (t ; DC..
      (R) (R)
      (sh4/print-output))))

(defun sh4/print-output ()
  (cond
    ((not (eq (cur-sym) #\C)) ; DC
      (pe2/append-2 ^(inst/print-full-config) #\0 #\:))
    (t ; DCC..
      (R) (R)
      (sh5/print-output))))

(defun sh5/print-output ()
  (cond
    ((not (eq (cur-sym) #\C)) ; DCC
      (pe2/append-2 ^(inst/print-full-config) #\1 #\:))
    (t
      (inst/print-full-config))))

(defun inst/print-full-config ()
  (g/find-last ^(l/move-left ^(inst1/print-full-config)) #\u))

(defun inst1/print-full-config ()
  (let ((a (cur-sym)))
    (R) (P #\Space)
    (case a
      (#\L (ce5/copy-all-erase ^(ov/over) #\v #\y #\x #\u #\w))
      (#\R (ce5/copy-all-erase ^(ov/over) #\v #\x #\u #\y #\w))
      (#\N (ce5/copy-all-erase ^(ov/over) #\v #\x #\y #\u #\w)))))

(defun ov/over ()
  (e/reset ^(pe/append ^(anf/advance-to-next) #\D)))
-}



