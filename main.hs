{-# LANGUAGE MultiWayIf #-}

import System.IO
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

type Table = ReaderT Handle (StateT Integer IO)

moveL :: Table ()
moveL = do
    i <- lift get
    lift $ put (i - 1)

moveR :: Table ()
moveR = do
    i <- lift get
    lift $ put (i + 1)

readS :: Table Char
readS = do
    f <- ask
    i <- lift get
    fileSize <- liftIO $ hFileSize f
    liftIO $ hSeek f AbsoluteSeek $ min i (fileSize - 1)
    liftIO $ hGetChar f

printS :: Char -> Table ()
printS c = do
    f <- ask
    i <- lift get
    liftIO $ hSeek f AbsoluteSeek i
    liftIO $ hPutChar f c

f :: Table () -> Table () -> Char -> Table ()
f k r a = readS >>= \s -> if 
    | s == 'E'  -> do moveL; f1 k r a
    | otherwise -> do moveL; f k r a

f1 :: Table () -> Table () -> Char -> Table ()
f1 k r a = readS >>= \s -> if
    | s == a    -> k
    | s == ' '  -> do moveR; f2 k r a
    | otherwise -> do moveR; f1 k r a

f2 :: Table () -> Table () -> Char -> Table ()
f2 k r a = readS >>= \s -> if
    | s == a    ->  k
    | s == ' '  -> do moveR; r
    | otherwise -> do moveR; f1 k r a

m :: Table ()
m = do
    f (return ()) (return ()) 'C'

main = do
    f <- openFile "tape" ReadWriteMode
    runStateT (runReaderT m f) 2
    hClose f


