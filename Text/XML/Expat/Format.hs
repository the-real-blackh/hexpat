{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Expat.Format (
        formatTree,
        formatTreeString,
        formatTreeByteString,
        formatTreeText,
        formatNode
    ) where

import Text.XML.Expat.IO
import Text.XML.Expat.Tree
import Control.Monad.Writer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Format document with <?xml.. header.
formatTree :: MonadWriter BSL.ByteString w =>
             (tag -> BSL.ByteString)  -- ^ Function to format a tag or attribute name
          -> (text -> BSL.ByteString) -- ^ Function to format XML text 
          -> Maybe Encoding
          -> Node tag text
          -> w ()
formatTree fmtTag fmtText mEnc node = do
    tell $ packL "<?xml version=\"1.0\""
    case mEnc of
        Just enc -> do
            tell $ packL " encoding=\""
            tell $ packL $ encodingToString enc
            tell $ packL "\""
        Nothing -> return ()
    tell $ packL "?>\n"
    formatNode fmtTag fmtText node
  where
    putEnc (Just enc) = (" encoding=\""++) . (encodingToString enc++) . ("\""++)
    putEnc Nothing = id

formatTreeString :: Maybe Encoding -> Node String String -> BSL.ByteString
formatTreeString mEnc node =
    execWriter $ formatTree packL packL mEnc node

formatTreeByteString :: Maybe Encoding -> Node BS.ByteString BS.ByteString -> BSL.ByteString
formatTreeByteString mEnc node =
    execWriter $ formatTree lazify lazify mEnc node

{-# INLINE lazify #-}
lazify bs = BSL.fromChunks [bs]

formatTreeText :: Maybe Encoding -> Node T.Text T.Text -> BSL.ByteString
formatTreeText mEnc node =
    execWriter $ formatTree encode encode mEnc node
  where
    encode = lazify . TE.encodeUtf8

-- | Format XML node with no XML header.
formatNode :: MonadWriter BSL.ByteString w =>
              (tag -> BSL.ByteString)  -- ^ Function to format a tag or attribute name
           -> (text -> BSL.ByteString) -- ^ Function to format XML text 
           -> Node tag text
           -> w ()
formatNode fmtTag fmtText (Element name attrs children) = do
    tell $ singL '<'
    let tagName = fmtTag name
    tell tagName
    forM attrs $ \(name, value) -> do
        tell $ singL ' '
        tell $ fmtTag name
        tell $ packL "=\""
        tell $ escapeXML $ fmtText value
        tell $ packL "\"" 
    if null children
        then
            tell $ packL "/>"
        else do
            tell $ singL '>'
            forM children $ formatNode fmtTag fmtText
            tell $ packL "</"
            tell tagName
            tell $ singL '>'
formatNode fmtTag fmtText (Text txt) =
    tell $ escapeXML $ fmtText txt

packL :: String -> BSL.ByteString
packL = BSL.pack . map c2w

singL :: Char -> BSL.ByteString
singL = BSL.singleton . c2w

unpackL :: BSL.ByteString -> String
unpackL = map w2c . BSL.unpack

escapeXML :: BSL.ByteString -> BSL.ByteString
escapeXML = packL . concatMap e . unpackL  -- to do: speed it up
    where
        e '&' = "&amp;"
        e '<' = "&lt;"
        e '"' = "&quot;"
        e '\'' = "&apos;"
        e ch = [ch]

