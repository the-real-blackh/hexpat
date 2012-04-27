{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, CPP, ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

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

  textFromCString,  -- ###
  gxFromCStringLen,  -- ###
  parse,
  parseG,
  parseLocations,
  parseLocationsG,
  parseLocationsThrowing,
  parseThrowing,
  defaultParseOptions,

  -- * Variants that throw exceptions
  XMLParseException(..),

  -- * Helpers
  setEntityDecoder,

  -- * Abstraction of string types
  GenericXMLString(..)
  ) where

import Control.Concurrent.MVar
import Text.XML.Expat.Internal.IO hiding (parse)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.Int
import Data.IORef
import Data.ByteString.Internal (c2w, w2c, c_strlen)
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Binary.UTF8.String as U8
import Data.List.Class hiding (tail)
import Data.Typeable
import Data.Word
import Control.Exception.Extensible as Exc
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import System.IO.Unsafe
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable


data ParseOptions tag text = ParseOptions
    { overrideEncoding :: Maybe Encoding
          -- ^ The encoding parameter, if provided, overrides the document's
          -- encoding declaration.
    , entityDecoder  :: Maybe (tag -> Maybe text)
          -- ^ If provided, entity references (i.e. @&nbsp;@ and friends) will
          -- be decoded into text using the supplied lookup function
    }

defaultParseOptions :: ParseOptions tag text
defaultParseOptions = ParseOptions Nothing Nothing


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
    gxFromByteString :: B.ByteString -> s
    gxToByteString :: s -> B.ByteString

instance GenericXMLString String where
    gxNullString = null
    gxToString = id
    gxFromString = id
    gxFromChar c = [c]
    gxHead = head
    gxTail = tail
    gxBreakOn c = break (==c)
    gxFromByteString = U8.decode . B.unpack
    gxToByteString = B.pack . map c2w . U8.encodeString

instance GenericXMLString B.ByteString where
    gxNullString = B.null
    gxToString = U8.decodeString . map w2c . B.unpack
    gxFromString = B.pack . map c2w . U8.encodeString
    gxFromChar = B.singleton . c2w
    gxHead = w2c . B.head
    gxTail = B.tail
    gxBreakOn c = B.break (== c2w c)
    gxFromByteString = id
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
    gxFromByteString = TE.decodeUtf8
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
    gxFromByteString <$> peekByteStringLen (cstr, fromIntegral len)

gxFromCStringLen :: GenericXMLString text => CStringLen -> IO text
gxFromCStringLen cl = gxFromByteString <$> peekByteStringLen cl

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

-- | Parse a generalized list of ByteStrings containing XML to SAX events.
-- In the event of an error, FailDocument is the last element of the output list.
parseG :: forall tag text l . (GenericXMLString tag, GenericXMLString text, List l) =>
          ParseOptions tag text -- ^ Parse options
       -> l ByteString          -- ^ Input text (a lazy ByteString)
       -> l (SAXEvent tag text)
{-# NOINLINE parseG #-}
parseG opts inputBlocks = runParser inputBlocks parser cacheRef
  where
    (parser, cacheRef) = unsafePerformIO $ do
        parser <- hexpatNewParser (overrideEncoding opts)
        {-
        case entityDecoder opts of
            Just deco -> setEntityDecoder parser deco $ \_ txt -> do
                modifyIORef queueRef (CharacterData txt:)
            Nothing -> return ()
            -}

        {-
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
            -}

        {-            
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
            -}

        cacheRef <- newMVar Nothing
        return (parser, cacheRef)

    runParser iblks parser cacheRef = joinL $ do
        li <- runList iblks
        return $ unsafePerformIO $ do
            mCached <- takeMVar cacheRef
            case mCached of
                Just l -> do
                    putMVar cacheRef mCached
                    return l
                Nothing -> do
                    (saxen, rema) <- case li of
                        Nil         -> do
                            (buf, len, mError) <- hexpatParse parser B.empty True
                            saxen <- parseBuf buf len
                            return (saxen, handleFailure mError mzero)
                        Cons blk t -> {-unsafeInterleaveIO $-} do
                            (buf, len, mError) <- hexpatParse parser blk False
                            saxen <- parseBuf buf len
                            cacheRef' <- newMVar Nothing
                            return (saxen, handleFailure mError (runParser t parser cacheRef'))
                    let l = fromList saxen `mplus` rema
                    putMVar cacheRef (Just l)
                    return l
      where
        handleFailure (Just err) _ = FailDocument err `cons` mzero
        handleFailure Nothing    l = l

parseBuf :: (GenericXMLString tag, GenericXMLString text) =>
            ForeignPtr Word8 -> CInt -> IO [SAXEvent tag text]
parseBuf buf _ = withForeignPtr buf $ \pBuf -> doit [] pBuf 0
  where
    roundUp32 offset = (offset + 3) .&. complement 3
    doit acc pBuf offset = offset `seq` do
        typ <- peek (pBuf `plusPtr` offset :: Ptr Word32)
        case typ of
            0 -> return (reverse acc)
            1 -> do
                nAtts <- peek (pBuf `plusPtr` (offset + 4) :: Ptr Word32)
                let pName = pBuf `plusPtr` (offset + 8)
                lName <- fromIntegral <$> c_strlen pName
                let name = gxFromByteString $ I.fromForeignPtr buf (offset + 8) lName
                (atts, offset') <- foldM (\(atts, offset) _ -> do
                        let pAtt = pBuf `plusPtr` offset
                        lAtt <- fromIntegral <$> c_strlen pAtt
                        let att = gxFromByteString $ I.fromForeignPtr buf offset lAtt
                            offset' = offset + lAtt + 1
                            pValue = pBuf `plusPtr` offset'
                        lValue <- fromIntegral <$> c_strlen pValue
                        let value = gxFromByteString $ I.fromForeignPtr buf offset' lValue
                        return ((att, value):atts, offset' + lValue + 1)
                    ) ([], offset + 8 + lName + 1) [1,3..nAtts]
                doit (StartElement name (reverse atts) : acc) pBuf (roundUp32 offset')
            2 -> do
                let pName = pBuf `plusPtr` (offset + 4)
                lName <- fromIntegral <$> c_strlen pName
                let name = gxFromByteString $ I.fromForeignPtr buf (offset + 4) lName
                    offset' = offset + 4 + lName + 1
                doit (EndElement name : acc) pBuf (roundUp32 offset')
            3 -> do
                len <- fromIntegral <$> peek (pBuf `plusPtr` (offset + 4) :: Ptr Word32)
                let text = gxFromByteString $ I.fromForeignPtr buf (offset + 8) len
                    offset' = offset + 8 + len
                doit (CharacterData text : acc) pBuf (roundUp32 offset')
            4 -> do
                let pEnc = pBuf `plusPtr` (offset + 4)
                lEnc <- fromIntegral <$> c_strlen pEnc
                let enc = gxFromByteString $ I.fromForeignPtr buf (offset + 4) lEnc
                    offset' = offset + 4 + lEnc + 1
                    pVer = pBuf `plusPtr` offset'
                pVerFirst <- peek (castPtr pVer :: Ptr Word8)
                (mVer, offset'') <- case pVerFirst of
                    0 -> return (Nothing, offset' + 1)
                    1 -> do
                        lVer <- fromIntegral <$> c_strlen (pVer `plusPtr` 1)
                        return (Just $ gxFromByteString $ I.fromForeignPtr buf (offset' + 1) lVer, offset' + 1 + lVer + 1)
                    _ -> error "hexpat: bad data from C land"
                cSta <- peek (pBuf `plusPtr` offset'' :: Ptr Int8)
                let sta = if cSta < 0  then Nothing else
                          if cSta == 0 then Just False else
                                            Just True
                doit (XMLDeclaration enc mVer sta : acc) pBuf (roundUp32 (offset'' + 1))
            _ -> error "hexpat: bad data from C land"

-- | Lazily parse XML to SAX events. In the event of an error, FailDocument is
-- the last element of the output list.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParseOptions tag text  -- ^ Parse options
      -> L.ByteString           -- ^ Input text (a lazy ByteString)
      -> [SAXEvent tag text]
parse opts input = parseG opts (L.toChunks input)


-- | An exception indicating an XML parse error, used by the /..Throwing/ variants.
data XMLParseException = XMLParseException XMLParseError
    deriving (Eq, Show, Typeable)

instance Exception XMLParseException where


-- | Parse a generalized list of ByteStrings containing XML to SAX events.
-- In the event of an error, FailDocument is the last element of the output list.
parseLocationsG :: forall tag text l . (GenericXMLString tag, GenericXMLString text, List l) =>
                   ParseOptions tag text -- ^ Parse options
                -> l ByteString          -- ^ Input text (a lazy ByteString)
                -> l (SAXEvent tag text, XMLParseLocation)
{-# NOINLINE parseLocationsG #-}
parseLocationsG opts inputBlocks = runParser inputBlocks parser queueRef cacheRef
  where
    (parser, queueRef, cacheRef) = unsafePerformIO $ do
        let enc = overrideEncoding opts
        let mEntityDecoder = entityDecoder opts

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
            modifyIORef queueRef ((XMLDeclaration ver mEnc sd, loc):)
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
            modifyIORef queueRef ((EndCData,loc):)
            return True
            
        setProcessingInstructionHandler parser $ \pp cTarget cText -> do
            target <- textFromCString cTarget
            txt <- textFromCString cText
            loc <- getParseLocation pp
            modifyIORef queueRef ((ProcessingInstruction target txt, loc):)
            return True
            
        setCommentHandler parser $ \pp cText -> do
            txt <- textFromCString cText
            loc <- getParseLocation pp
            modifyIORef queueRef ((Comment txt, loc):)
            return True

        cacheRef <- newIORef Nothing
        return (parser, queueRef, cacheRef)

    runParser iblks parser queueRef cacheRef = joinL $ do
        li <- runList iblks
        return $ unsafePerformIO $ do
            mCached <- readIORef cacheRef
            case mCached of
                Just l -> return l
                Nothing -> do
                    rema <- case li of
                            Nil         -> do
                                mError <- withParser parser $ \pp -> parseChunk pp B.empty True
                                handleFailure mError mzero
                            Cons blk t -> unsafeInterleaveIO $ do
                                mError <- withParser parser $ \pp -> parseChunk pp blk False
                                cacheRef' <- newIORef Nothing
                                handleFailure mError (runParser t parser queueRef cacheRef')
                    queue <- readIORef queueRef
                    writeIORef queueRef []
                    let l = fromList (reverse queue) `mplus` rema
                    writeIORef cacheRef (Just l)
                    return l
      where
        handleFailure (Just err) _ = do loc <- withParser parser getParseLocation
                                        return $ (FailDocument err, loc) `cons` mzero
        handleFailure Nothing    l = return l

-- | A variant of parseSAX that gives a document location with each SAX event.
parseLocations :: (GenericXMLString tag, GenericXMLString text) =>
                  ParseOptions tag text  -- ^ Parse options
               -> L.ByteString            -- ^ Input text (a lazy ByteString)
               -> [(SAXEvent tag text, XMLParseLocation)]
parseLocations opts input = parseLocationsG opts (L.toChunks input)


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
