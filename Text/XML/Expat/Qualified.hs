module Text.XML.Expat.Qualified (
        QName(..),
        qualifiedStringFlavor,
        qualifiedByteStringFlavor,
        qualifiedTextFlavor
    ) where

import Text.XML.Expat.IO
import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad.Writer
import Data.Monoid
import Data.Binary.Put
import qualified Codec.Binary.UTF8.String as U8


data QName text =
    QName {
        qnPrefix    :: Maybe text,
        qnLocalPart :: !text
    }
    deriving (Eq,Show)


qualifiedStringFlavor :: TreeFlavor (QName String) String
qualifiedStringFlavor = TreeFlavor (toQName . unpack) unpack fromQName pack
  where
    toQName ident =
        case break (== ':') ident of
            (prefix, ':':local) -> QName (Just prefix) local
            otherwise           -> QName Nothing ident
    unpack = U8.decodeString . map w2c . B.unpack
    pack = B.pack . map c2w . U8.encodeString
    fromQName (QName (Just prefix) local) = do
        mapM_ (putWord8 . c2w) prefix
        putWord8 $ c2w ':'
        mapM_ (putWord8 . c2w) local
    fromQName (QName Nothing local) = mapM_ (putWord8 . c2w) local

qualifiedByteStringFlavor :: TreeFlavor (QName B.ByteString) B.ByteString
qualifiedByteStringFlavor = TreeFlavor toQName id fromQName id
  where
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

qualifiedTextFlavor :: TreeFlavor (QName T.Text) T.Text
qualifiedTextFlavor = TreeFlavor (toQName . TE.decodeUtf8) TE.decodeUtf8 fromQName TE.encodeUtf8
  where
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


