import Control.Concurrent
import NetHelp
import Network
import System.IO

newConn :: Handle -> IO Conn
newConn h = newMVar [] >>= return . Conn h

main = withSocketsDo $ do
    conn <- newConn =<< startRoutine
    --waitForMsgs conn
    msgLoop conn
    where msgLoop c = displayMsgs c >>
        putStr ">: " >> getLine >>= sendMsg c >> msgLoop c

startRoutine :: IO Handle
startRoutine = do
    putStr "Where do you want to connect to?\n\t"
    host <- getLine
    putStr "What port do you want to connect to?\n\t"
    iPort <- getInt notAValidPortFn
    connectToServer host iPort

connectToServer :: HostName -> Int -> IO Handle
connectToServer host iPort = connectTo host (toPortID iPort)

-- Waits for a message to be received
--waitForMsgs :: Conn -> IO ()
--waitForMsgs c@(Conn h q) = forkIO (respLoop c) >> return ()
--    where respLoop c = hGetLine h >>= \resp ->
--        addMsgToQ resp c >> respLoop c
