import Network.WebSockets
import System.IO
import qualified Data.ByteString as BS hiding (pack)
import Data.ByteString.Char8 (pack)

main = runServer "127.0.0.1" 8080 handleConnection
  where
    handleConnection pending = do --handleConnection :: PendingConnection -> IO ()
        connection <- acceptRequest pending --calcRequest :: PendingConnection -> IO Connection
        let calcRequest = do
            commandMsg <- receiveDataMessage connection
            case commandMsg of
                _ -> do
                    sendTextData connection (pack "Hi! Your request is received!")
                    calcRequest
        calcRequest