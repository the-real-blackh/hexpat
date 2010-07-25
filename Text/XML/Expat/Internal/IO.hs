{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | Low-level interface to Expat. Unless speed is paramount, this should
-- normally be avoided in favour of the interfaces provided by
-- 'Text.XML.Expat.SAX' and 'Text.XML.Expat.Tree'.  Basic usage is:
--
-- (1) Make a new parser: 'newParser'.
--
-- (2) Set up callbacks on the parser: 'setStartElementHandler', etc.
--
-- (3) Feed data into the parser: 'parse', 'parse'' or 'parseChunk'.  Some of
--     these functions must be wrapped in 'withParser'.

module Text.XML.Expat.Internal.IO (
  -- ** Parser Setup
  Parser, newParser,

  -- ** Parsing
  parse, parse',
  withParser,
  ParserPtr, Parser_struct,
  parseChunk,
  Encoding(..),
  XMLParseError(..),
  getParseLocation,
  XMLParseLocation(..),

  -- ** Parser Callbacks
  StartElementHandler,
  EndElementHandler,
  CharacterDataHandler,
  ExternalEntityRefHandler,
  SkippedEntityHandler,
  setStartElementHandler,
  setEndElementHandler,
  setCharacterDataHandler,
  setExternalEntityRefHandler,
  setSkippedEntityHandler,
  setUseForeignDTD,

  -- ** Lower-level interface
  parseExternalEntityReference,
  ExpatHandlers,

  -- ** Helpers
  encodingToString
  ) where

import Control.Exception (bracket)
import Control.DeepSeq
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import Foreign
import Foreign.C


-- |Opaque parser type.
data Parser_struct
type ParserPtr = Ptr Parser_struct
data Parser = Parser
    { _parserObj                :: ForeignPtr Parser_struct
    , _startElementHandler      :: IORef CStartElementHandler
    , _endElementHandler        :: IORef CEndElementHandler
    , _cdataHandler             :: IORef CCharacterDataHandler
    , _externalEntityRefHandler :: IORef (Maybe CExternalEntityRefHandler)
    , _skippedEntityHandler     :: IORef (Maybe CSkippedEntityHandler)
    }

instance Show Parser where
    showsPrec _ (Parser fp _ _ _ _ _) = showsPrec 0 fp

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


parserCreate :: Maybe Encoding -> IO (ParserPtr)
parserCreate a1 =
  withOptEncoding a1 $ \a1' -> do
      pp <- parserCreate'_ a1'
      xmlSetUserData pp pp
      return pp

-- | Create a 'Parser'.
newParser :: Maybe Encoding -> IO Parser
newParser enc = do
  ptr        <- parserCreate enc
  fptr       <- newForeignPtr parserFree ptr
  nullStartH <- newIORef nullCStartElementHandler
  nullEndH   <- newIORef nullCEndElementHandler
  nullCharH  <- newIORef nullCCharacterDataHandler
  extH       <- newIORef Nothing
  skipH      <- newIORef Nothing

  return $ Parser fptr nullStartH nullEndH nullCharH extH skipH

setUseForeignDTD :: Parser -> Bool -> IO ()
setUseForeignDTD p b = withParser p $ \p' -> xmlUseForeignDTD p' b'
  where
    b' = if b then 1 else 0

-- ByteString.useAsCStringLen is almost what we need, but C2HS wants a CInt
-- instead of an Int.
withBStringLen :: BS.ByteString -> ((CString, CInt) -> IO a) -> IO a
withBStringLen bs f = do
  BS.useAsCStringLen bs $ \(str, len) -> f (str, fromIntegral len)

unStatus :: CInt -> Bool
unStatus 0 = False
unStatus _ = True

-- |@parse data@ feeds /lazy/ ByteString data into a 'Parser'. It returns
-- Nothing on success, or Just the parse error.
parse :: Parser -> BSL.ByteString -> IO (Maybe XMLParseError)
parse parser bs = withParser parser $ \pp -> do
    let
        doParseChunks [] = doParseChunk pp BS.empty True
        doParseChunks (c:cs) = do
            ok <- doParseChunk pp c False
            if ok
                then doParseChunks cs
                else return False
    ok <- doParseChunks (BSL.toChunks bs)
    if ok
        then return Nothing
        else Just `fmap` getError pp

-- |@parse data@ feeds /strict/ ByteString data into a 'Parser'. It returns
-- Nothing on success, or Just the parse error.
parse' :: Parser -> BS.ByteString -> IO (Maybe XMLParseError)
parse' parser bs = withParser parser $ \pp -> do
    ok <- doParseChunk pp bs True
    if ok
        then return Nothing
        else Just `fmap` getError pp

parseExternalEntityReference :: Parser
                             -> CString         -- ^ context
                             -> Maybe Encoding  -- ^ encoding
                             -> CStringLen      -- ^ text
                             -> IO Bool
parseExternalEntityReference parser context encoding (text,sz) =
    withParser parser $ \pp -> do
        extp <- withOptEncoding encoding $
                xmlExternalEntityParserCreate pp context
        e <- doParseChunk'_ extp text (fromIntegral sz) 1
        parserFree' extp
        return $ e == 1

-- |@parseChunk data False@ feeds /strict/ ByteString data into a
-- 'Parser'.  The end of the data is indicated by passing @True@ for the
-- final parameter.   It returns Nothing on success, or Just the parse error.
parseChunk :: ParserPtr
           -> BS.ByteString
           -> Bool
           -> IO (Maybe XMLParseError)
parseChunk pp xml final = do
    ok <- doParseChunk pp xml final
    if ok
        then return Nothing
        else Just `fmap` getError pp

getError :: ParserPtr -> IO XMLParseError
getError pp = do
    code <- xmlGetErrorCode pp
    cerr <- xmlErrorString code
    err <- peekCString cerr
    loc <- getParseLocation pp
    return $ XMLParseError err loc

data ExpatHandlers = ExpatHandlers
    (FunPtr CStartElementHandler)
    (FunPtr CEndElementHandler)
    (FunPtr CCharacterDataHandler)
    (Maybe (FunPtr CExternalEntityRefHandler))
    (Maybe (FunPtr CSkippedEntityHandler))

-- | Most of the low-level functions take a ParserPtr so are required to be
-- called inside @withParser@.
withParser :: Parser
           -> (ParserPtr -> IO a)  -- ^ Computation where parseChunk and other low-level functions may be used
           -> IO a
withParser parser@(Parser fp _ _ _ _ _) code = withForeignPtr fp $ \pp -> do
    bracket
        (unsafeSetHandlers parser pp)
        unsafeReleaseHandlers
        (\_ -> code pp)
  where
    unsafeSetHandlers :: Parser -> ParserPtr -> IO ExpatHandlers
    unsafeSetHandlers (Parser _ startRef endRef charRef extRef skipRef) pp =
      do
        cStartH <- mkCStartElementHandler =<< readIORef startRef
        cEndH   <- mkCEndElementHandler =<< readIORef endRef
        cCharH  <- mkCCharacterDataHandler =<< readIORef charRef
        mExtH   <- readIORef extRef >>=
                       maybe (return Nothing)
                             (\h -> liftM Just $ mkCExternalEntityRefHandler h)
    
        mSkipH  <- readIORef skipRef >>=
                       maybe (return Nothing)
                             (\h -> liftM Just $ mkCSkippedEntityHandler h)
    
        xmlSetstartelementhandler  pp cStartH
        xmlSetendelementhandler    pp cEndH
        xmlSetcharacterdatahandler pp cCharH
        maybe (return ())
              (xmlSetExternalEntityRefHandler pp)
              mExtH
        maybe (return ())
              (xmlSetSkippedEntityHandler pp)
              mSkipH
    
        return $ ExpatHandlers cStartH cEndH cCharH mExtH mSkipH
    
    unsafeReleaseHandlers :: ExpatHandlers -> IO ()
    unsafeReleaseHandlers (ExpatHandlers cStartH cEndH cCharH mcExtH mcSkipH) = do
        freeHaskellFunPtr cStartH
        freeHaskellFunPtr cEndH
        freeHaskellFunPtr cCharH
        maybe (return ()) freeHaskellFunPtr mcExtH
        maybe (return ()) freeHaskellFunPtr mcSkipH


-- |Obtain C value from Haskell 'Bool'.
--
cFromBool :: Num a => Bool -> a
cFromBool  = fromBool

doParseChunk :: ParserPtr -> BS.ByteString -> Bool -> IO (Bool)
doParseChunk a1 a2 a3 =
  withBStringLen a2 $ \(a2'1, a2'2) ->
  let {a3' = cFromBool a3} in
  doParseChunk'_ a1 a2'1  a2'2 a3' >>= \res ->
  let {res' = unStatus res} in
  return (res')

-- | Parse error, consisting of message text and error location
data XMLParseError = XMLParseError String XMLParseLocation deriving (Eq, Show)

instance NFData XMLParseError where
    rnf (XMLParseError msg loc) = rnf (msg, loc)

-- | Specifies a location of an event within the input text
data XMLParseLocation = XMLParseLocation {
        xmlLineNumber   :: Int64,  -- ^ Line number of the event
        xmlColumnNumber :: Int64,  -- ^ Column number of the event
        xmlByteIndex    :: Int64,  -- ^ Byte index of event from start of document
        xmlByteCount    :: Int64   -- ^ The number of bytes in the event
    }
    deriving (Eq, Show)

instance NFData XMLParseLocation where
    rnf (XMLParseLocation lin col ind cou) = rnf (lin, col, ind, cou)

getParseLocation :: ParserPtr -> IO XMLParseLocation
getParseLocation pp = do
    line <- xmlGetCurrentLineNumber pp
    col <- xmlGetCurrentColumnNumber pp
    index <- xmlGetCurrentByteIndex pp
    count <- xmlGetCurrentByteCount pp
    return $ XMLParseLocation {
            xmlLineNumber = fromIntegral line,
            xmlColumnNumber = fromIntegral col,
            xmlByteIndex = fromIntegral index,
            xmlByteCount = fromIntegral count
        }

-- | The type of the \"element started\" callback.  The first parameter is the
-- element name; the second are the (attribute, value) pairs. Return True to
-- continue parsing as normal, or False to terminate the parse.
type StartElementHandler  = ParserPtr -> CString -> [(CString, CString)] -> IO Bool

-- | The type of the \"element ended\" callback.  The parameter is the element
-- name. Return True to continue parsing as normal, or False to terminate the
-- parse.
type EndElementHandler    = ParserPtr -> CString -> IO Bool

-- | The type of the \"character data\" callback.  The parameter is the
-- character data processed.  This callback may be called more than once while
-- processing a single conceptual block of text. Return True to continue
-- parsing as normal, or False to terminate the parse.
type CharacterDataHandler = ParserPtr -> CStringLen -> IO Bool

-- | The type of the \"external entity reference\" callback. See the expat
-- documentation.
type ExternalEntityRefHandler =  Parser
                              -> CString   -- context
                              -> CString   -- base
                              -> CString   -- systemID
                              -> CString   -- publicID
                              -> IO Bool

-- | Set a skipped entity handler. This is called in two situations:
--
-- 1. An entity reference is encountered for which no declaration has been read
-- and this is not an error.
--
-- 2. An internal entity reference is read, but not expanded, because
-- @XML_SetDefaultHandler@ has been called.
type SkippedEntityHandler =  ParserPtr
                          -> CString   -- entityName
                          -> Int       -- is a parameter entity?
                          -> IO Bool

type CStartElementHandler = ParserPtr -> CString -> Ptr CString -> IO ()

nullCStartElementHandler :: CStartElementHandler
nullCStartElementHandler _ _ _ = return ()

foreign import ccall safe "wrapper"
  mkCStartElementHandler :: CStartElementHandler
                         -> IO (FunPtr CStartElementHandler)

wrapStartElementHandler :: Parser -> StartElementHandler -> CStartElementHandler
wrapStartElementHandler parser handler = h
  where
    h pp cname cattrs = do
        cattrlist <- peekArray0 nullPtr cattrs
        stillRunning <- handler pp cname (pairwise cattrlist)
        unless stillRunning $ stopp parser

-- | Attach a StartElementHandler to a Parser.
setStartElementHandler :: Parser -> StartElementHandler -> IO ()
setStartElementHandler parser@(Parser _ startRef _ _ _ _) handler =
    writeIORef startRef $ wrapStartElementHandler parser handler

type CEndElementHandler = ParserPtr -> CString -> IO ()

nullCEndElementHandler :: CEndElementHandler
nullCEndElementHandler _ _ = return ()

foreign import ccall safe "wrapper"
  mkCEndElementHandler :: CEndElementHandler
                       -> IO (FunPtr CEndElementHandler)
wrapEndElementHandler :: Parser -> EndElementHandler -> CEndElementHandler
wrapEndElementHandler parser handler = h
  where
    h pp cname = do
        stillRunning <- handler pp cname
        unless stillRunning $ stopp parser

-- | Attach an EndElementHandler to a Parser.
setEndElementHandler :: Parser -> EndElementHandler -> IO ()
setEndElementHandler parser@(Parser _ _ endRef _ _ _) handler =
    writeIORef endRef $ wrapEndElementHandler parser handler

type CCharacterDataHandler = ParserPtr -> CString -> CInt -> IO ()

nullCCharacterDataHandler :: CCharacterDataHandler
nullCCharacterDataHandler _ _ _ = return ()

foreign import ccall safe "wrapper"
  mkCCharacterDataHandler :: CCharacterDataHandler
                          -> IO (FunPtr CCharacterDataHandler)
wrapCharacterDataHandler :: Parser -> CharacterDataHandler -> CCharacterDataHandler
wrapCharacterDataHandler parser handler = h
  where
    h pp cdata len = do
        stillRunning <- handler pp (cdata, fromIntegral len)
        unless stillRunning $ stopp parser

-- | Attach an CharacterDataHandler to a Parser.
setCharacterDataHandler :: Parser -> CharacterDataHandler -> IO ()
setCharacterDataHandler parser@(Parser _ _ _ charRef _ _) handler =
    writeIORef charRef $ wrapCharacterDataHandler parser handler

pairwise :: [a] -> [(a,a)]
pairwise (x1:x2:xs) = (x1,x2) : pairwise xs
pairwise _          = []

stopp :: Parser -> IO ()
stopp parser = withParser parser $ \p -> xmlStopParser p 0

------------------------------------------------------------------------------
-- C imports

foreign import ccall unsafe "XML_ParserCreate"
  parserCreate'_ :: Ptr CChar -> IO ParserPtr
  
foreign import ccall unsafe "XML_SetUserData"
  xmlSetUserData :: ParserPtr -> ParserPtr -> IO ()

foreign import ccall unsafe "XML_SetStartElementHandler"
  xmlSetstartelementhandler :: ParserPtr -> ((FunPtr (ParserPtr -> ((Ptr CChar) -> ((Ptr (Ptr CChar)) -> (IO ())))) -> (IO ())))

foreign import ccall unsafe "XML_SetEndElementHandler"
  xmlSetendelementhandler :: ParserPtr -> ((FunPtr (ParserPtr -> ((Ptr CChar) -> (IO ()))) -> (IO ())))

foreign import ccall unsafe "XML_SetCharacterDataHandler"
  xmlSetcharacterdatahandler :: ParserPtr -> ((FunPtr (ParserPtr -> ((Ptr CChar) -> (CInt -> (IO ())))) -> (IO ())))

foreign import ccall safe "XML_Parse"
  doParseChunk'_ :: ParserPtr -> ((Ptr CChar) -> (CInt -> (CInt -> (IO CInt))))

foreign import ccall unsafe "XML_UseForeignDTD"
  xmlUseForeignDTD :: ParserPtr     -- ^ parser
                   -> CChar         -- ^ use foreign DTD? (external entity ref
                                    -- handler will be called with publicID &
                                    -- systemID set to null
                   -> IO ()

foreign import ccall "&XML_ParserFree" parserFree :: FunPtr (ParserPtr -> IO ())
foreign import ccall "XML_ParserFree" parserFree' :: ParserPtr -> IO ()

type CExternalEntityRefHandler = ParserPtr   -- parser
                              -> Ptr CChar   -- context
                              -> Ptr CChar   -- base
                              -> Ptr CChar   -- systemID
                              -> Ptr CChar   -- publicID
                              -> IO ()

foreign import ccall safe "wrapper"
  mkCExternalEntityRefHandler :: CExternalEntityRefHandler
                              -> IO (FunPtr CExternalEntityRefHandler)


foreign import ccall unsafe "XML_SetExternalEntityRefHandler"
  xmlSetExternalEntityRefHandler :: ParserPtr
                                 -> FunPtr CExternalEntityRefHandler
                                 -> IO ()

foreign import ccall unsafe "XML_SetSkippedEntityHandler"
  xmlSetSkippedEntityHandler :: ParserPtr
                             -> FunPtr CSkippedEntityHandler
                             -> IO ()

foreign import ccall unsafe "XML_ExternalEntityParserCreate"
  xmlExternalEntityParserCreate :: ParserPtr
                                -> CString   -- ^ context
                                -> CString   -- ^ encoding
                                -> IO ParserPtr

type CSkippedEntityHandler =  ParserPtr -- user data pointer
                           -> CString   -- entity name
                           -> CInt      -- is a parameter entity?
                           -> IO ()

foreign import ccall safe "wrapper"
  mkCSkippedEntityHandler :: CSkippedEntityHandler
                          -> IO (FunPtr CSkippedEntityHandler)


wrapExternalEntityRefHandler :: Parser
                             -> ExternalEntityRefHandler
                             -> CExternalEntityRefHandler
wrapExternalEntityRefHandler parser handler = h
  where
    h _ context base systemID publicID = do
        stillRunning <- handler parser context base systemID publicID
        unless stillRunning $ stopp parser


wrapSkippedEntityHandler :: Parser
                         -> SkippedEntityHandler
                         -> CSkippedEntityHandler
wrapSkippedEntityHandler parser handler = h
  where
    h pp entityName i = do
        stillRunning <- handler pp entityName (fromIntegral i)
        unless stillRunning $ stopp parser


setExternalEntityRefHandler :: Parser -> ExternalEntityRefHandler -> IO ()
setExternalEntityRefHandler parser h =
    writeIORef ref $ Just $ wrapExternalEntityRefHandler parser h
  where
    ref = _externalEntityRefHandler parser

setSkippedEntityHandler :: Parser -> SkippedEntityHandler -> IO ()
setSkippedEntityHandler parser h =
    writeIORef ref $ Just $ wrapSkippedEntityHandler parser h
  where
    ref = _skippedEntityHandler parser

-- Note on word sizes:
--
-- on expat 2.0:
-- XML_GetCurrentLineNumber returns XML_Size
-- XML_GetCurrentColumnNumber returns XML_Size
-- XML_GetCurrentByteIndex returns XML_Index
-- These are defined in expat_external.h
--
-- debian-i386 says XML_Size and XML_Index are 4 bytes.
-- ubuntu-amd64 says XML_Size and XML_Index are 8 bytes.
-- These two systems do NOT define XML_LARGE_SIZE, which would force these types
-- to be 64-bit.
--
-- If we guess the word size too small, it shouldn't matter: We will just discard
-- the most significant part.  If we get the word size too large, we will get
-- garbage (very bad).
--
-- So - what I will do is use CLong and CULong, which correspond to what expat
-- is using when XML_LARGE_SIZE is disabled, and give the correct sizes on the
-- two machines mentioned above.  At the absolute worst the word size will be too
-- short.

foreign import ccall unsafe "expat.h XML_GetErrorCode" xmlGetErrorCode
    :: ParserPtr -> IO CInt
foreign import ccall unsafe "expat.h XML_GetCurrentLineNumber" xmlGetCurrentLineNumber
    :: ParserPtr -> IO CULong
foreign import ccall unsafe "expat.h XML_GetCurrentColumnNumber" xmlGetCurrentColumnNumber
    :: ParserPtr -> IO CULong
foreign import ccall unsafe "expat.h XML_GetCurrentByteIndex" xmlGetCurrentByteIndex
    :: ParserPtr -> IO CLong
foreign import ccall unsafe "expat.h XML_GetCurrentByteCount" xmlGetCurrentByteCount
    :: ParserPtr -> IO CInt
foreign import ccall unsafe "expat.h XML_ErrorString" xmlErrorString
    :: CInt -> IO CString
foreign import ccall unsafe "expat.h XML_StopParser" xmlStopParser
    :: ParserPtr -> CInt -> IO ()

