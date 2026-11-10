module Network.Plan9Hask.Parser where

import Data.ByteString qualified
import Data.Attoparsec.ByteString qualified
import Data.Attoparsec qualified
import Data.Serialize.Get qualified
import Data.Word qualified

msgParser :: Data.Attoparsec.ByteString.Parser Data.Word.Word32
msgParser = do
    sizeBytes <- Data.Attoparsec.ByteString.take 4
    let parsedSize = Data.Serialize.Get.runGet Data.Serialize.Get.getWord32le sizeBytes
    size <- case parsedSize of
        (Right sizeWord) -> pure sizeWord
        (Left err) -> fail err
    return size

runMsgParser :: Data.ByteString.ByteString -> Data.Attoparsec.Result Data.Word.Word32
runMsgParser bytes = Data.Attoparsec.ByteString.parse msgParser bytes
