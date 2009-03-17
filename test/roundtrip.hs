{-# LANGUAGE ScopedTypeVariables #-}

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
  
  case parseTree' Nothing xml of
    Left uerr -> putStrLn $ show uerr
    Right (u::UNode String) -> do
      B.putStrLn $ formatTree' u
      B.putStrLn $ formatTree' $ fromQualified $ toQualified u
      B.putStrLn $ formatTree' $ fromQualified $ fromNamespaced $ toNamespaced $ toQualified u
