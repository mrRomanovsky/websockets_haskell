import Prelude hiding (span)
import Network.WebSockets --cabal install network, cabal install websockets
import System.IO
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import Data.Char (ord, isDigit)
import qualified Data.Map as M

mult = fromIntegral $ ord '*' :: Word8
minus = fromIntegral $ ord '-' :: Word8
divide = fromIntegral $ ord '/' :: Word8
plus = fromIntegral $ ord '+' :: Word8

ops = M.fromList [('*', (*)), ('-', (\a b -> a - b)), ('/', div), ('+', (+))]

calcExpr :: BSC.ByteString -> Maybe Int
calcExpr s = do
    let trimmed = BSC.dropWhile (not . isDigit) s
    (x, rs) <- BSC.readInt trimmed
    let op = BSC.head rs
        afterOp = BSC.tail rs
    o <- M.lookup op ops
    (y, _) <- BSC.readInt afterOp
    return $ x `o` y


main = runServer "127.0.0.1" 8080 handleConnection
  where
    handleConnection pending = do --handleConnection :: PendingConnection -> IO ()
        connection <- acceptRequest pending --calcRequest :: PendingConnection -> IO Connection
        let calcRequest = do
            commandMsg <- receiveData connection
            case calcExpr commandMsg of
                (Just x) -> sendTextData connection $ BSC.pack $ show x
            calcRequest
        calcRequest