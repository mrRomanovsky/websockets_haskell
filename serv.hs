import Prelude hiding (span)
import Network.WebSockets --cabal install network, cabal install websockets
import System.IO
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import Data.Char (ord, isDigit)
import qualified Data.Map as M
import Text.Parsec (parse, many1) --cabal install parsec
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (digit, char, string, spaces, endOfLine)
import Control.Applicative ((<|>))



ops = M.fromList [('*', (*)), ('-', (\a b -> a - b)), ('/', div), ('+', (+))]

calcExpr :: BSC.ByteString -> Maybe Int
calcExpr s = case parse parseExpr "" s of
                 Right (op, x, y) -> M.lookup op ops >>= \o -> Just $ o x y
                 _          -> Nothing

parseExpr :: Parser (Char, Int, Int)
parseExpr = do
    string "CALC"
    spaces
    op <- char '*' <|> char '/' <|> char '-' <|> char '+'
    spaces
    x <- many1 digit
    spaces
    y <- many1 digit
    spaces
    return (op, read x, read y)

main = do
    port <- read . head <$> getArgs
    putStrLn $ "Listening on port : " ++ show port
    runServer "127.0.0.1" port handleConnection
  where
    handleConnection pending = do --handleConnection :: PendingConnection -> IO ()
        connection <- acceptRequest pending --calcRequest :: PendingConnection -> IO Connection
        let calcRequest = do
            commandMsg <- receiveData connection
            case calcExpr commandMsg of
                (Just x) -> sendTextData connection $ BSC.pack $ "OK " ++ show x ++ " \n"
                _        -> sendTextData connection $ BSC.pack "ERR \n"
            calcRequest
        calcRequest