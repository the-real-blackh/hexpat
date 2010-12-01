{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, CPP #-}

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
  ParseOptions(..),
  SAXEvent(..),

  textFromCString,
  parse,
  parseLocations,
  parseLocationsThrowing,
  parseThrowing,
  defaultParseOptions,

  -- * Variants that throw exceptions
  XMLParseException(..),

  -- * Helpers
  setEntityDecoder,

  -- * Abstraction of string types
  GenericXMLString(..),

  -- * Deprecated parse functions
  parseSAX,
  parseSAXLocations,
  parseSAXLocationsThrowing,
  parseSAXThrowing,
  ParserOptions,
  defaultParserOptions
  ) where

import Text.XML.Expat.Internal.IO hiding (parse)
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
import Control.DeepSeq
import Control.Monad
import System.IO.Unsafe
import Foreign.C.String
import Foreign.Ptr


data ParseOptions tag text = ParseOptions
    { overrideEncoding :: Maybe Encoding
          -- ^ The encoding parameter, if provided, overrides the document's
          -- encoding declaration.
    , entityDecoder  :: Maybe (tag -> Maybe text)
          -- ^ If provided, entity references (i.e. @&nbsp;@ and friends) will
          -- be decoded into text using the supplied lookup function
    }

{-# DEPRECATED ParserOptions "renamed to ParseOptions" #-}
type ParserOptions tag text = ParseOptions tag text

defaultParseOptions :: ParseOptions tag text
defaultParseOptions = ParseOptions Nothing Nothing

-- | DEPRECATED. Renamed to defaultParseOptions.
defaultParserOptions :: ParseOptions tag text
{-# DEPRECATED defaultParserOptions "renamed to defaultParseOptions" #-}
defaultParserOptions = defaultParseOptions


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
#if MIN_VERSION_text(0,11,0)
    gxBreakOn c = T.break (==c)
#elif MIN_VERSION_text(0,10,0)
    -- breakBy gets renamed to break between 0.10.0.0 and 0.10.0.1.
    -- There's no 'break' function that is consistent between these two
    -- versions so we work around it using other functions.
    gxBreakOn c t = (T.takeWhile (/=c) t, T.dropWhile (/=c) t)
#else
    gxBreakOn c = T.breakBy (==c)
#endif
    gxFromCStringLen cstr = TE.decodeUtf8 <$> peekByteStringLen cstr
    gxToByteString = TE.encodeUtf8

peekByteStringLen :: CStringLen -> IO B.ByteString
{-# INLINE peekByteStringLen #-}
peekByteStringLen (cstr, len) =
    I.create (fromIntegral len) $ \ptr ->
        I.memcpy ptr (castPtr cstr) (fromIntegral len)


data SAXEvent tag text =
    XMLDeclaration text (Maybe text) (Maybe Bool) |
    StartElement tag [(tag, text)] |
    EndElement tag |
    CharacterData text |
    StartCData |
    EndCData |
    ProcessingInstruction text text |
    Comment text |
    FailDocument XMLParseError
    deriving (Eq, Show)

instance (NFData tag, NFData text) => NFData (SAXEvent tag text) where
    rnf (XMLDeclaration ver mEnc mSD) = rnf ver `seq` rnf mEnc `seq` rnf mSD
    rnf (StartElement tag atts) = rnf tag `seq` rnf atts
    rnf (EndElement tag) = rnf tag
    rnf (CharacterData text) = rnf text
    rnf StartCData = ()
    rnf EndCData = ()
    rnf (ProcessingInstruction target text) = rnf target `seq` rnf text
    rnf (Comment text) = rnf text
    rnf (FailDocument err) = rnf err

-- | Converts a 'CString' to a 'GenericXMLString' type.
textFromCString :: GenericXMLString text => CString -> IO text
{-# INLINE textFromCString #-}
textFromCString cstr = do
    len <- c_strlen cstr
    gxFromCStringLen (cstr, fromIntegral len)

-- | A helper for configuring the hexpat parser to use the specified entity
-- decoder.
setEntityDecoder :: (GenericXMLString tag, GenericXMLString text)
                 => Parser
                 -> (tag -> Maybe text)   -- ^ Entity decoder
                 -> (ParserPtr -> text -> IO ())       -- ^ Code to insert character data into the document
                 -> IO ()
setEntityDecoder parser decoder insertText = do
    setUseForeignDTD parser True
    setExternalEntityRefHandler parser eh
    setSkippedEntityHandler parser skip

  where
    skip _  _ 1 = return False
    skip pp entityName 0 = do
        en <- textFromCString entityName
        let mbt = decoder en
        maybe (return False)
              (\t -> do
                   insertText pp t
                   return True)
              mbt
    skip _ _ _ = undefined

    eh pp ctx _ systemID publicID =
        if systemID == nullPtr && publicID == nullPtr
           then withCStringLen "" $ \c -> do
               parseExternalEntityReference pp ctx Nothing c
           else return False

-- | Lazily parse XML to SAX events. In the event of an error, FailDocument is
-- the last element of the output list.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParseOptions tag text -- ^ Parse options
      -> L.ByteString           -- ^ Input text (a lazy ByteString)
      -> [SAXEvent tag text]
parse opts input = unsafePerformIO $ do
    let enc = overrideEncoding opts
    let mEntityDecoder = entityDecoder opts

    parser <- newParser enc
    queueRef <- newIORef []

    case mEntityDecoder of
        Just deco -> setEntityDecoder parser deco $ \_ txt -> do
            modifyIORef queueRef (CharacterData txt:)
        Nothing -> return ()

    setXMLDeclarationHandler parser $ \_ cVer cEnc cSd -> do
        ver <- textFromCString cVer
        mEnc <- if cEnc == nullPtr
            then return Nothing
            else Just <$> textFromCString cEnc
        let sd = if cSd < 0
                then Nothing
                else Just $ if cSd /= 0 then True else False
        modifyIORef queueRef (XMLDeclaration ver mEnc sd:)
        return True

    setStartElementHandler parser $ \_ cName cAttrs -> do
        name <- textFromCString cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- textFromCString cAttrName
            attrValue <- textFromCString cAttrValue
            return (attrName, attrValue)
        modifyIORef queueRef (StartElement name attrs:)
        return True

    setEndElementHandler parser $ \_ cName -> do
        name <- textFromCString cName
        modifyIORef queueRef (EndElement name:)
        return True

    setCharacterDataHandler parser $ \_ cText -> do
        txt <- gxFromCStringLen cText
        modifyIORef queueRef (CharacterData txt:)
        return True
        
    setStartCDataHandler parser $ \_  -> do
        modifyIORef queueRef (StartCData :)
        return True
        
    setEndCDataHandler parser $ \_  -> do
        modifyIORef queueRef (EndCData :)
        return True
        
    setProcessingInstructionHandler parser $ \_ cTarget cText -> do
        target <- textFromCString cTarget
        txt <- textFromCString cText
        modifyIORef queueRef (ProcessingInstruction target txt :)
        return True
        
    setCommentHandler parser $ \_ cText -> do
        txt <- textFromCString cText
        modifyIORef queueRef (Comment txt :)
        return True

    let runParser inp = unsafeInterleaveIO $ do
            rema <- withParser parser $ \pp -> case inp of
                (c:cs) -> do
                    mError <- parseChunk pp c False
                    case mError of
                        Just err -> return [FailDocument err]
                        Nothing -> runParser cs
                [] -> do
                    mError <- parseChunk pp B.empty True
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
parseSAX enc = parse (ParseOptions enc Nothing)


-- | An exception indicating an XML parse error, used by the /..Throwing/ variants.
data XMLParseException = XMLParseException XMLParseError
    deriving (Eq, Show, Typeable)

instance Exception XMLParseException where


-- | A variant of parseSAX that gives a document location with each SAX event.
parseLocations :: (GenericXMLString tag, GenericXMLString text) =>
                  ParseOptions tag text  -- ^ Parse options
               -> L.ByteString            -- ^ Input text (a lazy ByteString)
               -> [(SAXEvent tag text, XMLParseLocation)]
parseLocations opts input = unsafePerformIO $ do
    let enc = overrideEncoding opts
    let mEntityDecoder = entityDecoder opts

    -- Done with cut & paste coding for maximum speed.
    parser <- newParser enc
    queueRef <- newIORef []

    case mEntityDecoder of
        Just deco -> setEntityDecoder parser deco $ \pp txt -> do
            loc <- getParseLocation pp
            modifyIORef queueRef ((CharacterData txt, loc):)
        Nothing -> return ()

    setXMLDeclarationHandler parser $ \pp cVer cEnc cSd -> do
        ver <- textFromCString cVer
        mEnc <- if cEnc == nullPtr
            then return Nothing
            else Just <$> textFromCString cEnc
        let sd = if cSd < 0
                then Nothing
                else Just $ if cSd /= 0 then True else False
        loc <- getParseLocation pp
        modifyIORef queueRef ((XMLDeclaration ver mEnc sd,loc):)
        return True

    setStartElementHandler parser $ \pp cName cAttrs -> do
        name <- textFromCString cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- textFromCString cAttrName
            attrValue <- textFromCString cAttrValue
            return (attrName, attrValue)
        loc <- getParseLocation pp
        modifyIORef queueRef ((StartElement name attrs,loc):)
        return True

    setEndElementHandler parser $ \pp cName -> do
        name <- textFromCString cName
        loc <- getParseLocation pp
        modifyIORef queueRef ((EndElement name, loc):)
        return True

    setCharacterDataHandler parser $ \pp cText -> do
        txt <- gxFromCStringLen cText
        loc <- getParseLocation pp
        modifyIORef queueRef ((CharacterData txt, loc):)
        return True
        
    setStartCDataHandler parser $ \pp -> do
        loc <- getParseLocation pp
        modifyIORef queueRef ((StartCData, loc):)
        return True
        
    setEndCDataHandler parser $ \pp -> do
        loc <- getParseLocation pp
        modifyIORef queueRef ((EndCData, loc):)
        return True
        
    setProcessingInstructionHandler parser $ \pp cTarget cText -> do
        target <- textFromCString cTarget
        txt <- textFromCString cText
        loc <- getParseLocation pp
        modifyIORef queueRef ((ProcessingInstruction target txt, loc) :)
        return True
        
    setCommentHandler parser $ \pp cText -> do
        txt <- textFromCString cText
        loc <- getParseLocation pp
        modifyIORef queueRef ((Comment txt, loc) :)
        return True

    let runParser inp = unsafeInterleaveIO $ do
            rema <- withParser parser $ \pp -> case inp of
                (c:cs) -> do
                    mError <- parseChunk pp c False
                    case mError of
                        Just err -> do
                            loc <- getParseLocation pp
                            return [(FailDocument err, loc)]
                        Nothing -> runParser cs
                [] -> do
                    mError <- parseChunk pp B.empty True
                    case mError of
                        Just err -> do
                            loc <- getParseLocation pp
                            return [(FailDocument err, loc)]
                        Nothing -> return []
            queue <- readIORef queueRef
            writeIORef queueRef []
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
parseSAXLocations enc = parseLocations (ParseOptions enc Nothing)


-- | Lazily parse XML to SAX events. In the event of an error, throw
-- 'XMLParseException'.
--
-- @parseThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                 ParseOptions tag text  -- ^ Parse options
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
parseSAXThrowing mEnc = parseThrowing (ParseOptions mEnc Nothing)


-- | A variant of parseSAX that gives a document location with each SAX event.
-- In the event of an error, throw 'XMLParseException'.
--
-- @parseLocationsThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseLocationsThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                          ParseOptions tag text  -- ^ Optional encoding override
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
    parseLocationsThrowing (ParseOptions mEnc Nothing)
