import Control.Concurrent
import Control.Exception
import Data.Char
import Network
--import Network.Info
import System.IO

data Conn = Conn { connHand :: Handle, msgQ :: MVar [String] }

newConn :: Handle -> IO Conn
newConn h = newMVar [] >>= return . Conn h

main = withSocketsDo $ do
    conn <- newConn =<< startRoutine
    --waitForMsgs conn
    msgLoop conn
    where msgLoop c = displayMsgs c >> putStr ">: " >> getLine >>= sendMsg c >> msgLoop c

-- Sends out non-null messages, the flushes the handle
sendMsg :: Conn -> String -> IO ()
sendMsg (Conn h _) msg
    | not $ null msg = hPutStrLn h msg >> hFlush h
    | otherwise = return ()

getInt :: (String -> String) -> IO Int
getInt errFn = getLine >>= \s ->
    if all isDigit s then return $ read s else putStrLn (errFn s) >> getInt errFn

startRoutine :: IO Handle
startRoutine = do
    putStr "Where do you want to connect to?\n\t"
    host <- getLine
    putStr "What port do you want to connect to?\n\t"
    iPort <- getInt (++ " is not a valid port")
    connectToServer host iPort

toPortID :: Integral i => i -> PortID
toPortID = PortNumber . fromIntegral

connectToServer :: HostName -> Int -> IO Handle
connectToServer host iPort = connectTo host (toPortID iPort)

-- Clears the queue, returns the messages, and sets the queue as empty
getMsgs :: Conn -> IO [String]
getMsgs c@(Conn h q) = takeMVar q >>= \msgs -> putMVar q [] >> return msgs

-- Shows all the received messages
displayMsgs :: Conn -> IO ()
displayMsgs c = getMsgs c >>= mapM_ (\m -> putStrLn $ "\t<: " ++ m)

-- Waits for a message to be received
--waitForMsgs :: Conn -> IO ()
--waitForMsgs c@(Conn h q) = forkIO (respLoop c) >> return ()
--    where respLoop c = hGetLine h >>= \resp ->
--        addMsgToQ resp c >> respLoop c
