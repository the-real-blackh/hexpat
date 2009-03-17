import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
import Text.XML.Expat.Namespaced
import qualified Data.ByteString as B

{-- | Load the XSD schema in using all three strategies - raw tags, qualified tags and namespaced tags.
    We are checking that thing are going according to plan. In particular, that namespace URIs are being substituted in correctly.
| --}
main = do
  --xml <- B.readFile "test.xml"
  xml <- B.readFile "XMLSchema.xsd"
  
  case parseTree' qualifiedByteStringFlavor Nothing xml of
    Left qerr -> putStrLn $ show qerr
    Right q -> do
      case parseTree' byteStringFlavor Nothing xml of
        Left terr -> putStrLn $ show terr
        Right t -> do
          let tq = toQualified t
          putStrLn . show $ q == tq