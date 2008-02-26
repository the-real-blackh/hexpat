import qualified Text.XML.Expat.Raw as Raw
import Text.XML.Expat.Stream

doc = "<foo baz='bah'><bar/><text>some &amp; text</text></foo>"


main_raw = do
  parser <- Raw.parserCreate Nothing
  Raw.setStartElementHandler parser startElement
  Raw.parse parser doc True
  Raw.parserFree parser
  putStrLn "ok"
  where
  startElement name attrs = putStrLn $ show name ++ " " ++ show attrs

main_stream = do
  let handlers = defaultHandlers {startElementHandler=Just startElement}
  case parse Nothing handlers doc [] of
    Left ()    -> putStrLn "parse error"
    Right tags -> print tags
  where
  startElement tag attrs st = st ++ [tag]

main = main_raw
