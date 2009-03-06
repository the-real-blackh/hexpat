module Text.XML.Expat.Qualified (
        QName(..),
        parseTreeQualifiedString,
        parseTreeQualifiedByteString,
        parseTreeQualifiedText,
        formatTreeQualifiedString,
        formatTreeQualifiedByteString,
        formatTreeQualifiedText
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
parseTreeQualifiedString :: Maybe Encoding
                        -> BSL.ByteString
                        -> Maybe (Node (QName String) String)
parseTreeQualifiedString = parseTree (toQName . unpack, unpack)
  where
    unpack = map w2c . BS.unpack
    toQName ident =
        case break (== ':') ident of
            (prefix, ':':local) -> QName (Just prefix) local
            otherwise           -> QName Nothing ident

-- | Parse to a tree of type Node ByteString ByteString
parseTreeQualifiedByteString :: Maybe Encoding
                            -> BSL.ByteString
                            -> Maybe (Node (QName BS.ByteString) BS.ByteString)
parseTreeQualifiedByteString = parseTree (toQName, id)
  where
    toQName ident =
        case BS.break (== c2w ':') ident of
            (prefix, _local) | not (BS.null _local) && BS.head _local == c2w ':' ->
                                   QName (Just prefix) (BS.tail _local)
            otherwise           -> QName Nothing ident

-- | Parse to a tree of type Node Text Text
parseTreeQualifiedText :: Maybe Encoding
                      -> BSL.ByteString
                      -> Maybe (Node (QName T.Text) T.Text)
parseTreeQualifiedText = parseTree (toQName . TE.decodeUtf8, TE.decodeUtf8)
  where
    toQName ident =
        case T.break (== ':') ident of
            (prefix, _local) | not (T.null _local) && T.head _local == ':' ->
                                   QName (Just prefix) (T.tail _local)
            otherwise           -> QName Nothing ident

packL :: String -> BSL.ByteString
packL = BSL.pack . map c2w

formatTreeQualifiedString :: Maybe Encoding -> Node (QName String) String -> BSL.ByteString
formatTreeQualifiedString mEnc node =
    execWriter $ formatTree (packL . fromQName) packL mEnc node
  where
    fromQName (QName (Just prefix) local) = prefix ++ ":" ++ local
    fromQName (QName Nothing local)       = local

formatTreeQualifiedByteString :: Maybe Encoding -> Node (QName BS.ByteString) BS.ByteString -> BSL.ByteString
formatTreeQualifiedByteString mEnc node =
    execWriter $ formatTree (lazify . fromQName) lazify mEnc node
  where
    fromQName (QName (Just prefix) local) = prefix `BS.append` colon `BS.append` local
    fromQName (QName Nothing local)       = local
    colon = BS.singleton (c2w ':')

{-# INLINE lazify #-}
lazify bs = BSL.fromChunks [bs]

formatTreeQualifiedText :: Maybe Encoding -> Node (QName T.Text) T.Text -> BSL.ByteString
formatTreeQualifiedText mEnc node =
    execWriter $ formatTree (encode . fromQName) encode mEnc node
  where
    encode = lazify . TE.encodeUtf8
    fromQName (QName (Just prefix) local) = prefix `T.append` colon `T.append` local
    fromQName (QName Nothing local)       = local
    colon = T.singleton ':'

