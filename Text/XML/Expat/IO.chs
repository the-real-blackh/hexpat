-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- |This module wraps the Expat API directly, with IO.

module Text.XML.Expat.IO (
  -- ** Parser Setup
  Parser, newParser,

  -- ** Parsing
  parse, Encoding(..),

  -- ** Parser Callbacks
  StartElementHandler, EndElementHandler, CharacterDataHandler,
  setStartElementHandler, setEndElementHandler, setCharacterDataHandler
) where

import C2HS

#include <expat.h>

-- Expat functions start with "XML", but C2HS appears to ignore our "as"
-- definitions if they only differ from the symbol in case.  So we write out
-- XML_* in most cases anyway...  :(
{#context lib = "expat" prefix = "XML"#}

-- |Opaque parser type.
type ParserPtr = Ptr ()
newtype Parser = Parser (ForeignPtr ())

withParser :: Parser -> (ParserPtr -> IO a) -> IO a
withParser (Parser fp) = withForeignPtr fp

-- |Encoding types available for the document encoding.
data Encoding = ASCII | UTF8 | UTF16 | ISO88591
encodingToString :: Encoding -> String
encodingToString ASCII    = "US-ASCII"
encodingToString UTF8     = "UTF-8"
encodingToString UTF16    = "UTF-16"
encodingToString ISO88591 = "ISO-8859-1"

withOptEncoding :: Maybe Encoding -> (CString -> IO a) -> IO a
withOptEncoding Nothing    f = f nullPtr
withOptEncoding (Just enc) f = withCString (encodingToString enc) f

{#fun unsafe XML_ParserCreate as parserCreate
    {withOptEncoding* `Maybe Encoding'} -> `ParserPtr' id#}
foreign import ccall "&XML_ParserFree" parserFree :: FunPtr (ParserPtr -> IO ())

-- |Create a 'Parser'.  The encoding parameter, if provided, overrides the
-- document's encoding declaration.
newParser :: Maybe Encoding -> IO Parser
newParser enc = do
  ptr <- parserCreate enc
  fptr <- newForeignPtr parserFree ptr
  return $ Parser fptr

unStatus :: CInt -> Bool
unStatus 0 = False
unStatus 1 = True
-- |@parse data False@ feeds mode data into a 'Parser'.  The end of the data
-- is indicated by passing True for the final parameter.  @parse@ returns
-- False on a parse error.
{#fun XML_Parse as parse
    {withParser* `Parser', `String' &, `Bool'} -> `Bool' unStatus#}

-- |The type of the \"element started\" callback.  The first parameter is
-- the element name; the second are the (attribute, value) pairs.
type StartElementHandler  = String -> [(String,String)] -> IO ()
-- |The type of the \"element ended\" callback.  The parameter is
-- the element name.
type EndElementHandler    = String -> IO ()
-- |The type of the \"character data\" callback.  The parameter is
-- the character data processed.  This callback may be called more than once
-- while processing a single conceptual block of text.
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
-- |Attach a StartElementHandler to a Parser.
setStartElementHandler :: Parser -> StartElementHandler -> IO ()
setStartElementHandler parser handler = do
  withParser parser $ \p -> do
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
-- |Attach an EndElementHandler to a Parser.
setEndElementHandler :: Parser -> EndElementHandler -> IO ()
setEndElementHandler parser handler = do
  withParser parser $ \p -> do
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
-- |Attach an CharacterDataHandler to a Parser.
setCharacterDataHandler :: Parser -> CharacterDataHandler -> IO ()
setCharacterDataHandler parser handler = do
  withParser parser $ \p -> do
  handler' <- wrapCharacterDataHandler handler
  {#call unsafe XML_SetCharacterDataHandler as ^#} p handler'

pairwise (x1:x2:xs) = (x1,x2) : pairwise xs
pairwise []         = []
