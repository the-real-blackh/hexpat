{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
-- | A typeclass to allow for functions that work with different node types
-- such as the ones defined in /Tree/ and /Annotated/.
module Text.XML.Expat.NodeClass where

import Data.Monoid (Monoid)
import Text.XML.Expat.SAX (GenericXMLString)


class NodeClass n where
    type NodeMonad n :: * -> *
    type NodeContainer n :: * -> *

    -- | Is the given node an element?
    isElement :: n tag text -> Bool

    -- | Is the given node text?
    isText :: n tag text -> Bool

    -- | Extract all text content from inside a tag into a single string, including
    -- any text contained in children.
    textContent :: Monoid text => n tag text -> text

    -- | Is the given node a tag with the given name?
    isNamed :: Eq tag => tag -> n tag text -> Bool

    -- | Get the name of this node if it's an element, return empty string otherwise.
    getName :: GenericXMLString tag => n tag text -> tag

    -- | Get the attributes of a node if it's an element, return empty list otherwise.
    getAttributes :: n tag text -> [(tag,text)]

    -- | Get children of a node if it's an element, return empty list otherwise.
    getChildren :: n tag text -> NodeContainer n (n tag text)

    -- | Modify name if it's an element, no-op otherwise.
    modifyName :: (tag -> tag)
               -> n tag text
               -> n tag text

    -- | Modify attributes if it's an element, no-op otherwise.
    modifyAttributes :: ([(tag, text)] -> [(tag, text)])
                     -> n tag text
                     -> n tag text

    -- | Modify children (non-recursively) if it's an element, no-op otherwise.
    modifyChildren :: (NodeContainer n (n tag text) -> NodeContainer n (n tag text))
                   -> n tag text
                   -> n tag text

    -- | Map all tags (both tag names and attribute names) recursively.
    mapAllTags :: (tag -> tag')
               -> n tag text
               -> n tag' text

    -- | Map an element non-recursively, allowing the tag type to be changed.
    mapElement :: ((tag, [(tag, text)], NodeContainer n (n tag text))
                       -> (tag', [(tag', text)], NodeContainer n (n tag' text)))
                  -> n tag text
                  -> n tag' text

-- | Get the value of the attribute having the specified name.
getAttribute :: (NodeClass n, GenericXMLString tag) => n tag text -> tag -> Maybe text
getAttribute n t = lookup t $ getAttributes n

-- | Set the value of the attribute with the specified name to the value, overwriting
-- the first existing attribute with that name if present.
setAttribute :: (Eq tag, NodeClass n, GenericXMLString tag) => tag -> text -> n tag text -> n tag text
setAttribute t newValue = modifyAttributes set
  where
    set [] = [(t, newValue)]
    set ((name, _):atts) | name == t = (name, newValue):atts
    set (att:atts) = att:set atts

-- | Delete the first attribute matching the specified name.
deleteAttribute :: (Eq tag, NodeClass n, GenericXMLString tag) => tag -> n tag text -> n tag text
deleteAttribute t = modifyAttributes del
  where
    del [] = []
    del ((name, _):atts) | name == t = atts
    del (att:atts) = att:del atts

-- | setAttribute if /Just/, deleteAttribute if /Nothing/.
alterAttribute :: (Eq tag, NodeClass n, GenericXMLString tag) => tag -> Maybe text -> n tag text -> n tag text
alterAttribute t (Just newValue) = setAttribute t newValue
alterAttribute t Nothing = deleteAttribute t

