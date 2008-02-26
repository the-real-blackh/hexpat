import Text.XML.Expat.Raw

startElement name attrs = do
  print name
  print attrs

main = do
  parser <- parserCreate "UTF-8"
  --setHandlers parser (handlers {startElementHandler=Just startElement})
  setStartElementHandler parser startElement
  parse parser "<foo baz='bah'><bar/></foo>" True
  putStrLn "ok"
