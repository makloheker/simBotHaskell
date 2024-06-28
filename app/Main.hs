{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)

data ApiResponse = ApiResponse
    { message :: T.Text
    } deriving (Show)

instance FromJSON ApiResponse where
    parseJSON = withObject "ApiResponse" $ \v ->
        ApiResponse <$> v .: "message"

sendRequest :: T.Text -> IO (Either String T.Text)
sendRequest inputText = do
    let request = setRequestMethod "POST"
                $ setRequestHost "api.simsimi.vn"
                $ setRequestPath "/v1/simtalk"
                $ setRequestSecure True
                $ setRequestPort 443
                $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
                $ setRequestBodyURLEncoded [("text", B.pack (T.unpack inputText)), ("lc", "id")]
                $ defaultRequest
    response <- httpLBS request
    let responseBody = getResponseBody response
    case eitherDecode responseBody :: Either String ApiResponse of
        Left err -> return $ Left err
        Right res -> return $ Right (message res)

main :: IO ()
main = do
    let loop = do
            TIO.putStr "you>: "
            hFlush stdout  
            inputText <- TIO.getLine
            if inputText `elem` ["exit", "quit", "keluar"]
                then TIO.putStrLn "Mengakhiri percakapan..."
                else do
                    result <- sendRequest inputText
                    case result of
                        Left err -> TIO.putStrLn $ "Error: " <> T.pack err
                        Right msg -> TIO.putStrLn $ "bot>: " <> msg
                    loop
    loop
