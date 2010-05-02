{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to parse an XML document to a lazy
-- stream of SAX events.
module Text.XML.Expat.SAX (
  -- * XML primitives
  Encoding(..),
  XMLParseError(..),
  XMLParseLocation(..),

  -- * SAX-style parse
  ParserOptions(..),
  SAXEvent(..),

  textFromCString,
  parse,
  parseLocations,
  parseLocationsThrowing,
  parseThrowing,
  defaultParserOptions,

  -- * Variants that throw exceptions
  XMLParseException(..),

  -- * Deprecated parse functions
  parseSAX,
  parseSAXLocations,
  parseSAXLocationsThrowing,
  parseSAXThrowing,

  -- * Abstraction of string types
  GenericXMLString(..)
) where

import Text.XML.Expat.IO hiding (parse)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.IORef
import Data.ByteString.Internal (c2w, w2c, c_strlen)
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Binary.UTF8.String as U8
import Data.Typeable
import Control.Exception.Extensible as Exc
import Control.Applicative
import Control.Parallel.Strategies
import Control.Monad
import System.IO.Unsafe
import Foreign.C.String
import Foreign.Ptr


data ParserOptions tag text = ParserOptions
    { parserEncoding :: Maybe Encoding
          -- ^ The encoding parameter, if provided, overrides the document's
          -- encoding declaration.
    , entityDecoder  :: Maybe (tag -> Maybe text)
          -- ^ If provided, entity references (i.e. @&nbsp;@ and friends) will
          -- be decoded into text using the supplied lookup function
    }

defaultParserOptions :: ParserOptions tag text
defaultParserOptions = ParserOptions Nothing Nothing


-- | An abstraction for any string type you want to use as xml text (that is,
-- attribute values or element text content). If you want to use a
-- new string type with /hexpat/, you must make it an instance of
-- 'GenericXMLString'.
class (M.Monoid s, Eq s) => GenericXMLString s where
    gxNullString :: s -> Bool
    gxToString :: s -> String
    gxFromString :: String -> s
    gxFromChar :: Char -> s
    gxHead :: s -> Char
    gxTail :: s -> s
    gxBreakOn :: Char -> s -> (s, s)
    gxFromCStringLen :: CStringLen -> IO s
    gxToByteString :: s -> B.ByteString

instance GenericXMLString String where
    gxNullString = null
    gxToString = id
    gxFromString = id
    gxFromChar c = [c]
    gxHead = head
    gxTail = tail
    gxBreakOn c = break (==c)
    gxFromCStringLen cstr = U8.decodeString <$> peekCStringLen cstr
    gxToByteString = B.pack . map c2w . U8.encodeString

instance GenericXMLString B.ByteString where
    gxNullString = B.null
    gxToString = U8.decodeString . map w2c . B.unpack
    gxFromString = B.pack . map c2w . U8.encodeString
    gxFromChar = B.singleton . c2w
    gxHead = w2c . B.head
    gxTail = B.tail
    gxBreakOn c = B.break (== c2w c)
    gxFromCStringLen = peekByteStringLen
    gxToByteString = id

instance GenericXMLString T.Text where
    gxNullString = T.null
    gxToString = T.unpack
    gxFromString = T.pack
    gxFromChar = T.singleton
    gxHead = T.head
    gxTail = T.tail
    gxBreakOn c = T.breakBy (==c)
    gxFromCStringLen cstr = TE.decodeUtf8 <$> peekByteStringLen cstr
    gxToByteString = TE.encodeUtf8

peekByteStringLen :: CStringLen -> IO B.ByteString
{-# INLINE peekByteStringLen #-}
peekByteStringLen (cstr, len) =
    I.create (fromIntegral len) $ \ptr ->
        I.memcpy ptr (castPtr cstr) (fromIntegral len)


data SAXEvent tag text =
    StartElement tag [(tag, text)] |
    EndElement tag |
    CharacterData text |
    FailDocument XMLParseError
    deriving (Eq, Show)

instance (NFData tag, NFData text) => NFData (SAXEvent tag text) where
    rnf (StartElement tag atts) = rnf (tag, atts)
    rnf (EndElement tag) = rnf tag
    rnf (CharacterData text) = rnf text
    rnf (FailDocument err) = rnf err


-- | Converts a 'CString' to a 'GenericXMLString' type.
textFromCString :: GenericXMLString text => CString -> IO text
{-# INLINE textFromCString #-}
textFromCString cstr = do
    len <- c_strlen cstr
    gxFromCStringLen (cstr, fromIntegral len)


setEntityDecoder :: (GenericXMLString tag, GenericXMLString text)
                 => Parser
                 -> IORef [SAXEvent tag text]
                 -> (tag -> Maybe text)
                 -> IO ()
setEntityDecoder parser queueRef decoder = do
    setUseForeignDTD parser True
    setExternalEntityRefHandler parser eh
    setSkippedEntityHandler parser skip

  where
    skip _ 1 = return False
    skip entityName 0 = do
        en <- textFromCString entityName
        let mbt = decoder en
        maybe (return False)
              (\t -> do
                   modifyIORef queueRef (CharacterData t:)
                   return True)
              mbt
    skip _ _ = undefined

    eh p ctx _ systemID publicID =
        if systemID == nullPtr && publicID == nullPtr
           then withCStringLen "" $ \c -> do
               parseExternalEntityReference p ctx Nothing c
           else return False


setEntityDecoderLoc :: (GenericXMLString tag, GenericXMLString text)
                    => Parser
                    -> IORef [(SAXEvent tag text, XMLParseLocation)]
                    -> (tag -> Maybe text)
                    -> IO ()
setEntityDecoderLoc parser queueRef decoder = do
    setUseForeignDTD parser True
    setExternalEntityRefHandler parser eh
    setSkippedEntityHandler parser skip

  where
    skip _ 1 = return False
    skip entityName 0 = do
        en <- textFromCString entityName
        let mbt = decoder en
        maybe (return False)
              (\t -> do
                   loc <- getParseLocation parser
                   modifyIORef queueRef ((CharacterData t,loc):)
                   return True)
              mbt
    skip _ _ = undefined

    eh p ctx _ systemID publicID =
        if systemID == nullPtr && publicID == nullPtr
           then withCStringLen "" $ \c -> do
               parseExternalEntityReference p ctx Nothing c
           else return False


-- | Lazily parse XML to SAX events. In the event of an error, FailDocument is
-- the last element of the output list.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParserOptions tag text -- ^ Parser options
      -> L.ByteString           -- ^ Input text (a lazy ByteString)
      -> [SAXEvent tag text]
parse opts input = unsafePerformIO $ do
    let enc = parserEncoding opts
    let mEntityDecoder = entityDecoder opts

    parser <- newParser enc
    queueRef <- newIORef []

    maybe (return ())
          (setEntityDecoder parser queueRef)
          mEntityDecoder

    setStartElementHandler parser $ \cName cAttrs -> do
        name <- textFromCString cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- textFromCString cAttrName
            attrValue <- textFromCString cAttrValue
            return (attrName, attrValue)
        modifyIORef queueRef (StartElement name attrs:)
        return True

    setEndElementHandler parser $ \cName -> do
        name <- textFromCString cName
        modifyIORef queueRef (EndElement name:)
        return True

    setCharacterDataHandler parser $ \cText -> do
        txt <- gxFromCStringLen cText
        modifyIORef queueRef (CharacterData txt:)
        return True

    let runParser inp = unsafeInterleaveIO $ do
            rema <- case inp of
                (c:cs) -> do
                    mError <- parseChunk parser c False
                    case mError of
                        Just err -> return [FailDocument err]
                        Nothing -> runParser cs
                [] -> do
                    mError <- parseChunk parser B.empty True
                    case mError of
                        Just err -> return [FailDocument err]
                        Nothing -> return []
            queue <- readIORef queueRef
            writeIORef queueRef []
            return $ reverse queue ++ rema

    runParser $ L.toChunks input


-- | DEPRECATED: Use 'parse' instead.
--
-- Lazily parse XML to SAX events. In the event of an error, FailDocument is
-- the last element of the output list. Deprecated in favour of new
-- 'Text.XML.Expat.SAX.parse'
parseSAX :: (GenericXMLString tag, GenericXMLString text) =>
            Maybe Encoding      -- ^ Optional encoding override
         -> L.ByteString        -- ^ Input text (a lazy ByteString)
         -> [SAXEvent tag text]
{-# DEPRECATED parseSAX "use Text.XML.Expat.SAX.parse instead" #-}
parseSAX enc = parse (ParserOptions enc Nothing)


-- | An exception indicating an XML parse error, used by the /..Throwing/ variants.
data XMLParseException = XMLParseException XMLParseError
    deriving (Eq, Show, Typeable)

instance Exception XMLParseException where


-- | A variant of parseSAX that gives a document location with each SAX event.
parseLocations :: (GenericXMLString tag, GenericXMLString text) =>
                  ParserOptions tag text  -- ^ Parser options
               -> L.ByteString            -- ^ Input text (a lazy ByteString)
               -> [(SAXEvent tag text, XMLParseLocation)]
parseLocations opts input = unsafePerformIO $ do
    let enc = parserEncoding opts
    let mEntityDecoder = entityDecoder opts

    -- Done with cut & paste coding for maximum speed.
    parser <- newParser enc
    queueRef <- newIORef []

    maybe (return ())
          (setEntityDecoderLoc parser queueRef)
          mEntityDecoder

    setStartElementHandler parser $ \cName cAttrs -> do
        name <- textFromCString cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- textFromCString cAttrName
            attrValue <- textFromCString cAttrValue
            return (attrName, attrValue)
        loc <- getParseLocation parser
        modifyIORef queueRef ((StartElement name attrs,loc):)
        return True

    setEndElementHandler parser $ \cName -> do
        name <- textFromCString cName
        loc <- getParseLocation parser
        modifyIORef queueRef ((EndElement name, loc):)
        return True

    setCharacterDataHandler parser $ \cText -> do
        txt <- gxFromCStringLen cText
        loc <- getParseLocation parser
        modifyIORef queueRef ((CharacterData txt, loc):)
        return True

    let runParser [] = return []
        runParser (c:cs) = unsafeInterleaveIO $ do
            mError <- parseChunk parser c (null cs)
            queue <- readIORef queueRef
            writeIORef queueRef []
            rema <- case mError of
                Just err -> do
                    loc <- getParseLocation parser
                    return [(FailDocument err, loc)]
                Nothing -> runParser cs
            return $ reverse queue ++ rema

    runParser $ L.toChunks input


-- | DEPRECATED: Use 'parseLocations' instead.
--
-- A variant of parseSAX that gives a document location with each SAX event.
parseSAXLocations :: (GenericXMLString tag, GenericXMLString text) =>
            Maybe Encoding      -- ^ Optional encoding override
         -> L.ByteString        -- ^ Input text (a lazy ByteString)
         -> [(SAXEvent tag text, XMLParseLocation)]
{-# DEPRECATED parseSAXLocations "use Text.XML.Expat.SAX.parseLocations instead" #-}
parseSAXLocations enc = parseLocations (ParserOptions enc Nothing)


-- | Lazily parse XML to SAX events. In the event of an error, throw
-- 'XMLParseException'.
--
-- @parseThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                 ParserOptions tag text  -- ^ Parser options
              -> L.ByteString            -- ^ input text (a lazy ByteString)
              -> [SAXEvent tag text]
parseThrowing opts bs = map freakOut $ parse opts bs
  where
    freakOut (FailDocument err) = Exc.throw $ XMLParseException err
    freakOut other = other


-- | DEPRECATED: Use 'parseThrowing' instead.
--
-- Lazily parse XML to SAX events. In the event of an error, throw
-- 'XMLParseException'.
parseSAXThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                    Maybe Encoding      -- ^ Optional encoding override
                 -> L.ByteString        -- ^ Input text (a lazy ByteString)
                 -> [SAXEvent tag text]
{-# DEPRECATED parseSAXThrowing "use Text.XML.Expat.SAX.parseThrowing instead" #-}
parseSAXThrowing mEnc = parseThrowing (ParserOptions mEnc Nothing)


-- | A variant of parseSAX that gives a document location with each SAX event.
-- In the event of an error, throw 'XMLParseException'.
--
-- @parseLocationsThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseLocationsThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                          ParserOptions tag text  -- ^ Optional encoding override
                       -> L.ByteString            -- ^ Input text (a lazy ByteString)
                       -> [(SAXEvent tag text, XMLParseLocation)]
parseLocationsThrowing opts bs = map freakOut $ parseLocations opts bs
  where
    freakOut (FailDocument err, _) = Exc.throw $ XMLParseException err
    freakOut other = other


-- | DEPRECATED: Used 'parseLocationsThrowing' instead.
--
-- A variant of parseSAX that gives a document location with each SAX event.
-- In the event of an error, throw 'XMLParseException'.
parseSAXLocationsThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                             Maybe Encoding      -- ^ Optional encoding override
                          -> L.ByteString        -- ^ Input text (a lazy ByteString)
                          -> [(SAXEvent tag text, XMLParseLocation)]
{-# DEPRECATED parseSAXLocationsThrowing "use Text.XML.Expat.SAX.parseLocationsThrowing instead" #-}
parseSAXLocationsThrowing mEnc =
    parseLocationsThrowing (ParserOptions mEnc Nothing)
