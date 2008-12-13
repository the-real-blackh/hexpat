import Text.XML.Expat.IO
import Text.XML.Expat.Tree as T
import Text.XML.Expat.Format
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Maybe

toByteString :: String -> B.ByteString
toByteString = B.pack . map (fromIntegral . ord)

test :: String -> IO ()
test xml = do
  let mTree = T.parse (Just UTF8) (toByteString xml)
  putStrLn $ show mTree
  case mTree of
      Just tree ->
          putStrLn $ formatDoc (Just UTF8) tree
      Nothing ->
          putStrLn "parse failed!"

main = do
  test "<?xml version=\"1.0\"?>\n<test><test1 type=\"expression\">Cat &amp; mouse</test1><test2 type=\"communication\">Dog &amp; bone</test2></test>"
  test "<?xml version=\"1.0\"?>\n<test>Cat &amp; mouse</test>"
