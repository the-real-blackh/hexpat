module Text.XML.Expat.Qualified (
        QName(..),
        parseQualifiedDocString,
        parseQualifiedDocByteString,
        parseQualifiedDocText,
        formatQualifiedDocString,
        formatQualifiedDocByteString,
        formatQualifiedDocText
    ) where

import Text.XML.Expat.IO
import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad.Writer
import Data.Monoid


data QName text =
    QName {
        qnPrefix    :: Maybe text,
        qnLocalPart :: !text
    }
    deriving (Eq,Show)

-- | Parse to a tree of type Node String String
parseQualifiedDocString :: Maybe Encoding
                        -> BSL.ByteString
                        -> Maybe (Node (QName String) String)
parseQualifiedDocString = parseDoc (toQName . unpack) unpack
  where
    unpack = map w2c . BS.unpack
    toQName ident =
        case break (== ':') ident of
            (prefix, ':':local) -> QName (Just prefix) local
            otherwise           -> QName Nothing ident

-- | Parse to a tree of type Node ByteString ByteString
parseQualifiedDocByteString :: Maybe Encoding
                            -> BSL.ByteString
                            -> Maybe (Node (QName BS.ByteString) BS.ByteString)
parseQualifiedDocByteString = parseDoc toQName id
  where
    toQName ident =
        case BS.break (== c2w ':') ident of
            (prefix, _local) | not (BS.null _local) && BS.head _local == c2w ':' ->
                                   QName (Just prefix) (BS.tail _local)
            otherwise           -> QName Nothing ident

-- | Parse to a tree of type Node Text Text
parseQualifiedDocText :: Maybe Encoding
                      -> BSL.ByteString
                      -> Maybe (Node (QName T.Text) T.Text)
parseQualifiedDocText = parseDoc (toQName . TE.decodeUtf8) TE.decodeUtf8
  where
    toQName ident =
        case T.break (== ':') ident of
            (prefix, _local) | not (T.null _local) && T.head _local == ':' ->
                                   QName (Just prefix) (T.tail _local)
            otherwise           -> QName Nothing ident

packL :: String -> BSL.ByteString
packL = BSL.pack . map c2w

formatQualifiedDocString :: Maybe Encoding -> Node (QName String) String -> BSL.ByteString
formatQualifiedDocString mEnc node =
    execWriter $ formatDoc (packL . fromQName) packL mEnc node
  where
    fromQName (QName (Just prefix) local) = prefix ++ ":" ++ local
    fromQName (QName Nothing local)       = local

formatQualifiedDocByteString :: Maybe Encoding -> Node (QName BS.ByteString) BS.ByteString -> BSL.ByteString
formatQualifiedDocByteString mEnc node =
    execWriter $ formatDoc (lazify . fromQName) lazify mEnc node
  where
    fromQName (QName (Just prefix) local) = prefix `BS.append` colon `BS.append` local
    fromQName (QName Nothing local)       = local
    colon = BS.singleton (c2w ':')

{-# INLINE lazify #-}
lazify bs = BSL.fromChunks [bs]

formatQualifiedDocText :: Maybe Encoding -> Node (QName T.Text) T.Text -> BSL.ByteString
formatQualifiedDocText mEnc node =
    execWriter $ formatDoc (encode . fromQName) encode mEnc node
  where
    encode = lazify . TE.encodeUtf8
    fromQName (QName (Just prefix) local) = prefix `T.append` colon `T.append` local
    fromQName (QName Nothing local)       = local
    colon = T.singleton ':'

