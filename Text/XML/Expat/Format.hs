module Text.XML.Expat.Format (
        formatDoc,
        formatDocS,
        formatNode,
        formatNodeS
    ) where

import Text.XML.Expat.IO
import Text.XML.Expat.Tree

enc :: String -> String
enc = id

-- | Format document with <?xml.. header.
formatDoc :: Maybe Encoding -> Node -> String
formatDoc mEnc node = formatDocS mEnc node ""

-- | Format document with <?xml.. header returning as a ShowS.
formatDocS :: Maybe Encoding -> Node -> ShowS
formatDocS mEnc node = ("<?xml version=\"1.0\""++) . putEnc mEnc . ("?>\n"++) . formatNodeS node
    where
        putEnc (Just enc) = (" encoding=\""++) . (encodingToString enc++) . ("\""++)
        putEnc Nothing = id

-- | Format XML node with no XML header.
formatNode :: Node -> String
formatNode node = formatNodeS node ""

-- | Format XML node with no XML header returning as a ShowS.
formatNodeS :: Node -> ShowS
formatNodeS (Element name attrs children) =
    ("<"++) . (enc name++) .
    foldr (.) id (flip map attrs (\(name, value) ->
            (" "++) . (enc name++) . ("=\""++) .
                     (enc (escapeXML value)++) . ("\""++) 
        )) .
    (if null children
            then
                ("/>"++)
            else
                (">"++) .
                foldr (.) id (map formatNodeS children) .
                ("</"++) . (enc name++) . (">"++)
        )
formatNodeS (Text txt) = (enc (escapeXML txt)++)

escapeXML :: String -> String
escapeXML = concatMap e
    where
        e '&' = "&amp;"
        e '<' = "&lt;"
        e '"' = "&quot;"
        e '\'' = "&apos;"
        e ch = [ch]
