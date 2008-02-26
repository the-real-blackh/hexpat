module Text.XML.Expat.Raw (
  Parser, parserCreate, parserFree, parse,
  StartElementHandler, EndElementHandler, CharacterDataHandler,
  setStartElementHandler, setEndElementHandler, setCharacterDataHandler
) where
import C2HS

#include <expat.h>

-- Expat functions start with "XML", but C2HS appears to ignore our "as"
-- definitions if they only differ from the symbol in case.  So we write out
-- XML_* in most cases anyway...  :(
{#context lib = "expat" prefix = "XML"#}

newtype Parser = Parser {#type Parser#}
asParser :: Ptr () -> Parser
asParser ptr = Parser ptr
unParser :: Parser -> Ptr ()
unParser (Parser p) = p
{#fun unsafe XML_ParserCreate as parserCreate {`String'} -> `Parser' asParser#}
{#fun unsafe XML_ParserFree as parserFree {unParser `Parser'} -> `()'#}

{#fun XML_Parse as parse
    {unParser `Parser', `String' &, `Bool'} -> `Int'#}

type StartElementHandler  = String -> [(String,String)] -> IO ()
type EndElementHandler    = String -> IO ()
type CharacterDataHandler = String -> IO ()

type CStartElementHandler = Ptr () -> CString -> Ptr CString -> IO ()
foreign import ccall "wrapper"
  mkCStartElementHandler :: CStartElementHandler
                         -> IO (FunPtr CStartElementHandler)
wrapStartElementHandler :: StartElementHandler
                        -> IO (FunPtr CStartElementHandler)
wrapStartElementHandler handler = mkCStartElementHandler h where
  h ptr cname cattrs = do
    name <- peekCString cname
    cattrlist <- peekArray0 nullPtr cattrs
    attrlist <- mapM peekCString cattrlist
    handler name (pairwise attrlist)
setStartElementHandler :: Parser -> StartElementHandler -> IO ()
setStartElementHandler (Parser p) handler = do
  handler' <- wrapStartElementHandler handler
  {#call unsafe XML_SetStartElementHandler as ^#} p handler'


type CEndElementHandler = Ptr () -> CString -> IO ()
foreign import ccall "wrapper"
  mkCEndElementHandler :: CEndElementHandler
                       -> IO (FunPtr CEndElementHandler)
wrapEndElementHandler :: EndElementHandler
                      -> IO (FunPtr CEndElementHandler)
wrapEndElementHandler handler = mkCEndElementHandler h where
  h ptr cname = do
    name <- peekCString cname
    handler name
setEndElementHandler :: Parser -> EndElementHandler -> IO ()
setEndElementHandler (Parser p) handler = do
  handler' <- wrapEndElementHandler handler
  {#call unsafe XML_SetEndElementHandler as ^#} p handler'

type CCharacterDataHandler = Ptr () -> CString -> CInt -> IO ()
foreign import ccall "wrapper"
  mkCCharacterDataHandler :: CCharacterDataHandler
                          -> IO (FunPtr CCharacterDataHandler)
wrapCharacterDataHandler :: CharacterDataHandler
                      -> IO (FunPtr CCharacterDataHandler)
wrapCharacterDataHandler handler = mkCCharacterDataHandler h where
  h ptr cdata len = do
    data_ <- peekCStringLen (cdata, fromIntegral len)
    handler data_
setCharacterDataHandler :: Parser -> CharacterDataHandler -> IO ()
setCharacterDataHandler (Parser p) handler = do
  handler' <- wrapCharacterDataHandler handler
  {#call unsafe XML_SetCharacterDataHandler as ^#} p handler'

pairwise (x1:x2:xs) = (x1,x2) : pairwise xs
pairwise []         = []
