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
  
  case  parseTree' Nothing xml of
    Left terr -> putStrLn $ show terr
    Right t -> do
      vitalStats "text" t
      let q = toQualified t
      vitalStats "qualified" q
      let n = toNamespaced q
      vitalStats "namespaced" t

vitalStats :: (Show tag) => String -> Node tag String -> IO ()
vitalStats name n = do
  putStrLn $ "Processing tree: " ++ name
  display "  " "" n
  

display :: (Show tag) => String -> String -> Node tag String -> IO ()
display _ pfx t@(Text _) = putStrLn $ pfx ++ show t
display ind pfx (Element name attrs chldrn) = do
  putStrLn $ pfx ++ "Element: " ++ show name
  let pfx' = ind ++ pfx
  mapM_ (\a -> putStrLn $ pfx' ++ show a) attrs
  mapM_ (display ind pfx') chldrn
