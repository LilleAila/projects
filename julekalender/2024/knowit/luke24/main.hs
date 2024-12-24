import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS -- Conflict with System.IO.readFile
import Data.Word (Word8)
import System.IO

decrypt :: ByteString -> ByteString
decrypt encrypted = BS.pack (decryptBytes (BS.unpack encrypted) 0)
  where
    decryptBytes :: [Word8] -> Word8 -> [Word8]
    decryptBytes [] _ = []
    decryptBytes (e : es) prev =
      let decrypted = e `xor` prev
       in decrypted : decryptBytes es decrypted

main :: IO ()
main = do
  encryptedData <- BS.readFile "kryptert.txt"
  let decryptedData = decrypt encryptedData
  BS.writeFile "dekryptert.txt" decryptedData
