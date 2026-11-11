module Network.Plan9Hask.Parser where

import Data.ByteString qualified
import Data.Attoparsec.ByteString qualified
import Data.Serialize.Get qualified
import Data.Word qualified

msgParser :: Data.Word.Word32 ->  Data.Attoparsec.ByteString.Parser (Int, Data.ByteString.ByteString)
msgParser maxSize = do
    sizeBytes <- Data.Attoparsec.ByteString.take 4
    let parsedSize = Data.Serialize.Get.runGet Data.Serialize.Get.getWord32le sizeBytes
    size <- case parsedSize of
        (Right sizeWord) | sizeWord < 4+1+2 -> fail "size cannot fit the message type and tag"
        (Right sizeWord) | sizeWord > maxSize -> fail "size is larger than the maximum"
        (Right sizeWord) -> pure $ (fromIntegral sizeWord :: Int)
        (Left err) -> fail err
    msgBytes <- Data.Attoparsec.ByteString.take size
    return (size, msgBytes)

runMsgParser :: Data.ByteString.ByteString -> Data.Attoparsec.ByteString.Result (Int, Data.ByteString.ByteString)
runMsgParser bytes = Data.Attoparsec.ByteString.parse (msgParser 8192)  bytes
