module Text.XML.Expat.Qualified (
        QName(..),
        parseTreeQualifiedString,
        parseTreeQualifiedByteString,
        parseTreeQualifiedText,
        parseTreeQualifiedStringLazy,
        parseTreeQualifiedByteStringLazy,
        parseTreeQualifiedTextLazy,
        formatTreeQualifiedString,
        formatTreeQualifiedByteString,
        formatTreeQualifiedText
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


data QName text =
    QName {
        qnPrefix    :: Maybe text,
        qnLocalPart :: !text
    }
    deriving (Eq,Show)

-- | Parse to a tree of type Node String String
parseTreeQualifiedString :: Maybe Encoding
                         -> L.ByteString
                         -> Maybe (Node (QName String) String)
parseTreeQualifiedString = parseTree (toQName_String . unpack, unpack)

-- | Parse to a tree of type Node ByteString ByteString
parseTreeQualifiedByteString :: Maybe Encoding
                             -> L.ByteString
                             -> Maybe (Node (QName B.ByteString) B.ByteString)
parseTreeQualifiedByteString = parseTree (toQName_ByteString, id)

-- | Parse to a tree of type Node Text Text
parseTreeQualifiedText :: Maybe Encoding
                      -> L.ByteString
                      -> Maybe (Node (QName T.Text) T.Text)
parseTreeQualifiedText = parseTree (toQName_Text . TE.decodeUtf8, TE.decodeUtf8)

-- | Parse to a tree of type Node String String
parseTreeQualifiedStringLazy :: Maybe Encoding
                             -> L.ByteString
                             -> Node (QName String) String
parseTreeQualifiedStringLazy = parseTreeLazy (toQName_String . unpack, unpack)

-- | Parse to a tree of type Node ByteString ByteString
parseTreeQualifiedByteStringLazy :: Maybe Encoding
                                 -> L.ByteString
                                 -> Node (QName B.ByteString) B.ByteString
parseTreeQualifiedByteStringLazy = parseTreeLazy (toQName_ByteString, id)

-- | Parse to a tree of type Node Text Text
parseTreeQualifiedTextLazy :: Maybe Encoding
                           -> L.ByteString
                           -> Node (QName T.Text) T.Text
parseTreeQualifiedTextLazy = parseTreeLazy (toQName_Text . TE.decodeUtf8, TE.decodeUtf8)

unpack = map w2c . B.unpack
toQName_String ident =
    case break (== ':') ident of
        (prefix, ':':local) -> QName (Just prefix) local
        otherwise           -> QName Nothing ident

toQName_ByteString ident =
    case B.break (== c2w ':') ident of
        (prefix, _local) | not (B.null _local) && B.head _local == c2w ':' ->
                               QName (Just prefix) (B.tail _local)
        otherwise           -> QName Nothing ident

toQName_Text ident =
    case T.break (== ':') ident of
        (prefix, _local) | not (T.null _local) && T.head _local == ':' ->
                               QName (Just prefix) (T.tail _local)
        otherwise           -> QName Nothing ident

pack :: String -> B.ByteString
pack = B.pack . map c2w

formatTreeQualifiedString :: Maybe Encoding -> Node (QName String) String -> L.ByteString
formatTreeQualifiedString mEnc node =
    formatTree (fromQName, pack) mEnc node
  where
    fromQName (QName (Just prefix) local) = do
        mapM_ (putWord8 . c2w) prefix
        putWord8 $ c2w ':'
        mapM_ (putWord8 . c2w) local
    fromQName (QName Nothing local) = mapM_ (putWord8 . c2w) local

formatTreeQualifiedByteString :: Maybe Encoding -> Node (QName B.ByteString) B.ByteString -> L.ByteString
formatTreeQualifiedByteString mEnc node =
    formatTree (fromQName, id) mEnc node
  where
    fromQName (QName (Just prefix) local) = do
        putByteString prefix
        putWord8 $ c2w ':'
        putByteString local
    fromQName (QName Nothing local) = putByteString local
    colon = B.singleton (c2w ':')

{-# INLINE lazify #-}
lazify bs = L.fromChunks [bs]

formatTreeQualifiedText :: Maybe Encoding -> Node (QName T.Text) T.Text -> L.ByteString
formatTreeQualifiedText mEnc node =
    formatTree (fromQName, TE.encodeUtf8) mEnc node
  where
    fromQName (QName (Just prefix) local) = do
        putByteString . TE.encodeUtf8 $ prefix
        putWord8 $ c2w ':'
        putByteString . TE.encodeUtf8 $ local
    fromQName (QName Nothing local) = putByteString . TE.encodeUtf8 $ local
    colon = T.singleton ':'

