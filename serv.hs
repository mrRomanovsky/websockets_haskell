import Network.WebSockets --cabal install network, cabal install websockets
import System.IO
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import Data.Char (ord, isDigit)
import Language.Haskell.Interpreter --cabal install hint
import Control.Concurrent.MVar
import Control.Exception

--calcExpr :: BSC.ByteString -> Maybe Int
calcExpr s = let [_, op, x, y] = words $ BSC.unpack s
                 e =  x ++ " " ++ op ++ " " ++ y
             in runInterpreter $ setImports ["Prelude"] >> eval e

main = do
    let port =  28563
    putStrLn $ "Listening on port : " ++ show port
    connsCnt <- newMVar 0
    runServer "127.0.0.1" port (handleConnection connsCnt)
  where
    handleConnection connsCnt pending = do --handleConnection :: PendingConnection -> IO ()
        connection <- acceptRequest pending --calcRequest :: PendingConnection -> IO Connection
        cnt <- (+1) <$> takeMVar connsCnt
        putMVar connsCnt cnt
        putStrLn $ "connected clients: " ++ show cnt
        let calcRequest = do
            commandMsg <- receiveData connection
            res <- calcExpr commandMsg
            case res of
                (Right x) -> sendTextData connection $ BSC.pack $ "OK " ++ x ++ " \n"
                _        -> sendTextData connection $ BSC.pack "ERR \n"
            calcRequest
        catch calcRequest (\(SomeException _) -> do {cnt <- (subtract 1) <$> takeMVar connsCnt;
         putMVar connsCnt cnt;  putStrLn $ "connected clients: " ++ show cnt})
