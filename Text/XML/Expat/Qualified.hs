-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | With the default flavors for 'Text.XML.Expat.Tree.parseTree' and
-- 'Text.XML.Expat.Format.formatTree', qualified tag and attribute names such as
-- \<abc:hello\> are represented just as a string containing a colon, e.g.
-- \"abc:hello\".
--
-- This module provides flavors that handle these more intelligently, splitting
-- all tag and attribute names into their Prefix and LocalPart components.

module Text.XML.Expat.Qualified (
        QName(..),
        mkQName,
        qualifiedStringFlavor,
        qualifiedByteStringFlavor,
        qualifiedTextFlavor
    ) where

import Text.XML.Expat.IO
import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Applicative
import Control.Monad.Writer
import Control.Parallel.Strategies
import Data.Monoid
import Data.Binary.Put
import qualified Codec.Binary.UTF8.String as U8
import Foreign.C.String
import Foreign.Ptr


data QName text =
    QName {
        qnPrefix    :: Maybe text,
        qnLocalPart :: !text
    }
    deriving (Eq,Show)

instance NFData text => NFData (QName text) where
    rnf (QName pre loc) = rnf (pre, loc)

-- | Make a new QName from a prefix and localPart.
mkQName :: text -> text -> QName text
mkQName prefix localPart = QName (Just prefix) localPart

-- | Flavor for qualified tags, using String data type.
qualifiedStringFlavor :: TreeFlavor (QName String) String
qualifiedStringFlavor = TreeFlavor (\t -> toQName <$> unpack t) unpackLen fromQName pack
  where
    unpack    cstr = U8.decodeString <$> peekCString cstr
    unpackLen cstr = U8.decodeString <$> peekCStringLen cstr
    toQName ident =
        case break (== ':') ident of
            (prefix, ':':local) -> QName (Just prefix) local
            otherwise           -> QName Nothing ident
    pack = B.pack . map c2w . U8.encodeString
    fromQName (QName (Just prefix) local) = do
        mapM_ (putWord8 . c2w) prefix
        putWord8 $ c2w ':'
        mapM_ (putWord8 . c2w) local
    fromQName (QName Nothing local) = mapM_ (putWord8 . c2w) local

-- | Flavor for qualified tags, using ByteString data type, containing UTF-8 encoded Unicode.
qualifiedByteStringFlavor :: TreeFlavor (QName B.ByteString) B.ByteString
qualifiedByteStringFlavor = TreeFlavor (\t -> toQName <$> unpack t) unpackLen fromQName id
  where
    unpack    cstr = peekByteString cstr
    unpackLen cstr = peekByteStringLen cstr
    toQName ident =
        case B.break (== c2w ':') ident of
            (prefix, _local) | not (B.null _local) && B.head _local == c2w ':' ->
                                   QName (Just prefix) (B.tail _local)
            otherwise           -> QName Nothing ident
    fromQName (QName (Just prefix) local) = do
        putByteString prefix
        putWord8 $ c2w ':'
        putByteString local
    fromQName (QName Nothing local) = putByteString local
    colon = B.singleton (c2w ':')

-- | Flavor for qualified tags, using Text data type.
qualifiedTextFlavor :: TreeFlavor (QName T.Text) T.Text
qualifiedTextFlavor = TreeFlavor (\t -> toQName <$> unpack t) unpackLen fromQName TE.encodeUtf8
  where
    unpack    cstr = TE.decodeUtf8 <$> peekByteString cstr
    unpackLen cstr = TE.decodeUtf8 <$> peekByteStringLen cstr
    toQName ident =
        case T.break (== ':') ident of
            (prefix, _local) | not (T.null _local) && T.head _local == ':' ->
                                   QName (Just prefix) (T.tail _local)
            otherwise           -> QName Nothing ident
    fromQName (QName (Just prefix) local) = do
        putByteString . TE.encodeUtf8 $ prefix
        putWord8 $ c2w ':'
        putByteString . TE.encodeUtf8 $ local
    fromQName (QName Nothing local) = putByteString . TE.encodeUtf8 $ local
    colon = T.singleton ':'

peekByteString :: CString -> IO B.ByteString
{-# INLINE peekByteString #-}
peekByteString cstr = do
    len <- I.c_strlen cstr
    peekByteStringLen (castPtr cstr, fromIntegral len)

peekByteStringLen :: CStringLen -> IO B.ByteString 
{-# INLINE peekByteStringLen #-}
peekByteStringLen (cstr, len) =
    I.create (fromIntegral len) $ \ptr ->
        I.memcpy ptr (castPtr cstr) (fromIntegral len)


