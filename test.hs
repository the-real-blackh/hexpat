import qualified Text.XML.Expat.IO as EIO
import Text.XML.Expat.Stream

doc = "<foo baz='bah'><bar/><text>some &amp; text</text></foo>"


main_eio = do
  parser <- EIO.newParser Nothing
  EIO.setStartElementHandler parser startElement
  EIO.parse parser doc True
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

main = main_eio
