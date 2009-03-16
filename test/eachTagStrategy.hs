import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Qualified
import Text.XML.Expat.Namespaced
import qualified Data.ByteString as B

{-- | Load the XSD schema in using all three strategies - raw tags, qualified tags and namespaced tags.
    We are checking that thing are going according to plan. In particular, that namespace URIs are being substituted in correctly.
| --}
main = do
  xml <- B.readFile "test.xml"
  --xml <- B.readFile "XMLSchema.xsd"
  
  case  parseTree' byteStringFlavor Nothing xml of
    Left terr -> putStrLn $ show terr
    Right t -> vitalStats "text" t
  case parseTree' qualifiedByteStringFlavor Nothing xml of
    Left qerr -> putStrLn $ show qerr
    Right q -> do
      vitalStats "qualified" q
      vitalStats "namespaced" $ withNamespaces q

vitalStats :: (Show tag, Show text) => String -> Node tag text -> IO ()
vitalStats name n = do
  putStrLn $ "Processing tree: " ++ name
  display "  " "" n
  

display :: (Show tag, Show text) => String -> String -> Node tag text -> IO ()
display _ pfx t@(Text _) = putStrLn $ pfx ++ show t
display ind pfx (Element name attrs chldrn) = do
  putStrLn $ pfx ++ "Element: " ++ show name
  let pfx' = ind ++ pfx
  mapM_ (\a -> putStrLn $ pfx' ++ show a) attrs
  mapM_ (display ind pfx') chldrn