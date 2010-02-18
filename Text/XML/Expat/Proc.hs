-- | This module ported from Text.XML.Light.Proc
module Text.XML.Expat.Proc where

import Text.XML.Expat.Tree

import Data.Maybe(listToMaybe)

-- | Select only the elements from a list of XML content.
onlyElems          :: [Node tag text] -> [Node tag text]
onlyElems xs        = [ x | x@(Element _ _ _) <- xs ]

-- | Select only the text from a list of XML content.
onlyText           :: [Node tag text] -> [text]
onlyText xs         = [ x | Text x <- xs ]

-- | Find all immediate children with the given name.
findChildren       :: (GenericXMLString tag) => tag -> Node tag text -> [Node tag text]
findChildren q e    = filterChildren ((q ==) . eName) e

-- | Filter all immediate children wrt a given predicate.
filterChildren       :: (Node tag text -> Bool) -> Node tag text -> [Node tag text]
filterChildren _ (Text _) = []
filterChildren p e    = filter p (onlyElems (eChildren e))

-- | Filter all immediate children wrt a given predicate over their names.
filterChildrenName      :: (tag -> Bool) -> Node tag text -> [Node tag text]
filterChildrenName _ (Text _) = []
filterChildrenName p e   = filter (p . eName) (onlyElems (eChildren e))

-- | Find an immediate child with the given name.
findChild          :: (GenericXMLString tag) => tag -> Node tag text -> Maybe (Node tag text)
findChild q e       = listToMaybe (findChildren q e)

-- | Find an immediate child with the given name.
filterChild          :: (Node tag text -> Bool) -> Node tag text -> Maybe (Node tag text)
filterChild p e       = listToMaybe (filterChildren p e)

-- | Find an immediate child with name matching a predicate.
filterChildName      :: (tag -> Bool) -> Node tag text -> Maybe (Node tag text)
filterChildName p e   = listToMaybe (filterChildrenName p e)

-- | Find the left-most occurrence of an element matching given name.
findElement        :: (GenericXMLString tag) => tag -> Node tag text -> Maybe (Node tag text)
findElement q e     = listToMaybe (findElements q e)

-- | Filter the left-most occurrence of an element wrt. given predicate.
filterElement        :: (Node tag text -> Bool) -> Node tag text -> Maybe (Node tag text)
filterElement p e     = listToMaybe (filterElements p e)

-- | Filter the left-most occurrence of an element wrt. given predicate.
filterElementName     :: (tag -> Bool) -> Node tag text -> Maybe (Node tag text)
filterElementName p e  = listToMaybe (filterElementsName p e)

-- | Find all non-nested occurances of an element.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
findElements       :: (GenericXMLString tag) => tag -> Node tag text -> [Node tag text]
findElements qn e = filterElementsName (qn==) e

-- | Find all non-nested occurrences of an element wrt. given predicate.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
filterElements       :: (Node tag text -> Bool) -> Node tag text -> [Node tag text]
filterElements p e
 | p e        = [e]
 | otherwise  = case e of
                  Element _ _ c -> concatMap (filterElements p) $ onlyElems c
                  _             -> []

-- | Find all non-nested occurences of an element wrt a predicate over element names.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
filterElementsName       :: (tag -> Bool) -> Node tag text -> [Node tag text]
filterElementsName _ (Text _) = []
filterElementsName p e = filterElements (p.eName) e

