-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | In the default representation, qualified tag and attribute names such as
-- \<abc:hello\> are represented just as a string containing a colon, e.g.
-- \"abc:hello\".
--
-- This module provides functionality to handle these more intelligently, splitting
-- all tag and attribute names into their Prefix and LocalPart components.

module Text.XML.Expat.Qualified (
        QName(..),
        QNode,
        QNodes,
        QAttributes,
        mkQName,
        mkAnQName,
        toQualified,
        fromQualified
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
import qualified Codec.Binary.UTF8.String as U8

-- | A qualified name.
--
-- Qualified names have two parts, a prefix and a local part. The local part
-- is the name of the tag. The prefix scopes that name to a particular
-- group of legal tags.
--
-- The prefix will usually be associated with a namespace URI. This is usually
-- achieved by using xmlns attributes to bind prefixes to URIs.
data QName text =
    QName {
        qnPrefix    :: Maybe text,
        qnLocalPart :: !text
    }
    deriving (Eq,Show)

instance NFData text => NFData (QName text) where
    rnf (QName pre loc) = rnf (pre, loc)

-- | Type shortcut for nodes where qualified names are used for tags
type QNodes text = Nodes (QName text) text

-- | Type shortcut for a single node where qualified names are used for tags
type QNode text = Node (QName text) text

-- | Type shortcut for attributes with qualified names
type QAttributes text = Attributes (QName text) text

-- | Make a new QName from a prefix and localPart.
mkQName :: text -> text -> QName text
mkQName prefix localPart = QName (Just prefix) localPart

-- | Make a new QName with no prefix.
mkAnQName :: text -> QName text
mkAnQName localPart = QName Nothing localPart

toQualified :: (GenericXMLString text) => UNode text -> QNode text
toQualified (Text text) = Text text
toQualified (Element uname uatts uchldrn) = Element qname qatts qchldrn
  where
    qname   = qual uname
    qatts   = map (\(tag, text) -> (qual tag, text)) uatts
    qchldrn = map toQualified uchldrn

    qual ident =
        case gxBreakOn ':' ident of
             (prefix, _local) | not (gxNullString _local)
                              && gxHead _local == ':'
                                 -> QName (Just prefix) (gxTail _local)
             otherwise           -> QName Nothing ident

fromQualified :: (GenericXMLString text) => QNode text -> UNode text
fromQualified (Text text) = Text text
fromQualified (Element qname qatts qchldrn) = Element uname uatts uchldrn
  where
    uname   = tag qname
    uatts   = map (\(qual, text) -> (tag qual, text)) qatts
    uchldrn = map fromQualified qchldrn

    tag (QName (Just prefix) local) = prefix `mappend` gxFromChar ':' `mappend` local
    tag (QName Nothing       local) = local

