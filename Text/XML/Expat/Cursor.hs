--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Expat.Cursor
--
-- This module ported from Text.XML.Light.Cursor
--
-- XML cursors for working XML content withing the context of
-- an XML document.  This implementation is based on the general
-- tree zipper written by Krasimir Angelov and Iavor S. Diatchki.
--

module Text.XML.Expat.Cursor
  ( Tag(..), getTag, fromTag
  , Cursor(..), Path

  -- * Conversions
  , fromTree
  , fromForest
  , toForest
  , toTree

  -- * Moving around
  , parent
  , root
  , getChild
  , firstChild
  , lastChild
  , left
  , right
  , nextDF

  -- ** Searching
  , findChild
  , findLeft
  , findRight
  , findRec

  -- * Node classification
  , isRoot
  , isFirst
  , isLast
  , isLeaf
  , isChild
  , hasChildren
  , getNodeIndex

  -- * Updates
  , setContent
  , modifyContent
  , modifyContentList
  , modifyContentM

  -- ** Inserting content
  , insertLeft
  , insertRight
  , insertManyLeft
  , insertManyRight
  , insertFirstChild
  , insertLastChild
  , insertManyFirstChild
  , insertManyLastChild
  , insertGoLeft
  , insertGoRight

  -- ** Removing content
  , removeLeft
  , removeRight
  , removeGoLeft
  , removeGoRight
  , removeGoUp

  ) where

import Text.XML.Expat.Tree
import Data.Maybe(isNothing)
import Control.Monad(mplus)

data Tag tag text = Tag { tagName    :: tag
                        , tagAttribs :: Attributes tag text
                        } deriving (Show)

{-
setTag :: Tag -> Element -> Element
setTag t e = fromTag t (elContent e)
-}

fromTag :: Tag tag text -> [Node tag text] -> Node tag text
fromTag t cs = Element { eName       = tagName t
                       , eAttributes = tagAttribs t
                       , eChildren   = cs
                       }

type Path tag text = [([Node tag text],Tag tag text,[Node tag text])]

-- | The position of a piece of content in an XML document.
data Cursor tag text = Cur
  { current :: Node tag text      -- ^ The currently selected content.
  , lefts   :: [Node tag text]    -- ^ Siblings on the left, closest first.
  , rights  :: [Node tag text]    -- ^ Siblings on the right, closest first.
  , parents :: Path tag text -- ^ The contexts of the parent elements of this location.
  } deriving (Show)

-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: Cursor tag text -> Maybe (Cursor tag text)
parent loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Cur { current = (fromTag v
                    (combChildren (lefts loc) (current loc) (rights loc)))
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: Cursor tag text -> Cursor tag text
root loc = maybe loc root (parent loc)

-- | The left sibling of the given location.
left :: Cursor tag text -> Maybe (Cursor tag text)
left loc =
  case lefts loc of
    t : ts -> Just loc { current = t, lefts = ts
                                    , rights = current loc : rights loc }
    []     -> Nothing

-- | The right sibling of the given location.
right :: Cursor tag text -> Maybe (Cursor tag text)
right loc =
  case rights loc of
    t : ts -> Just loc { current = t, lefts = current loc : lefts loc
                                    , rights = ts }
    []     -> Nothing

-- | The first child of the given location.
firstChild :: Cursor tag text -> Maybe (Cursor tag text)
firstChild loc =
  do (t : ts, ps) <- downParents loc
     return Cur { current = t, lefts = [], rights = ts , parents = ps }

-- | The last child of the given location.
lastChild :: Cursor tag text -> Maybe (Cursor tag text)
lastChild loc =
  do (ts, ps) <- downParents loc
     case reverse ts of
       l : ls -> return Cur { current = l, lefts = ls, rights = []
                                                     , parents = ps }
       [] -> Nothing

-- | Find the next left sibling that satisfies a predicate.
findLeft :: (Cursor tag text -> Bool) -> Cursor tag text -> Maybe (Cursor tag text)
findLeft p loc = do loc1 <- left loc
                    if p loc1 then return loc1 else findLeft p loc1

-- | Find the next right sibling that satisfies a predicate.
findRight :: (Cursor tag text -> Bool) -> Cursor tag text -> Maybe (Cursor tag text)
findRight p loc = do loc1 <- right loc
                     if p loc1 then return loc1 else findRight p loc1

-- | The first child that satisfies a predicate.
findChild :: (Cursor tag text -> Bool) -> Cursor tag text -> Maybe (Cursor tag text)
findChild p loc =
  do loc1 <- firstChild loc
     if p loc1 then return loc1 else findRight p loc1

-- | The next position in a left-to-right depth-first traversal of a document:
-- either the first child, right sibling, or the right sibling of a parent that
-- has one.
nextDF :: Cursor tag text -> Maybe (Cursor tag text)
nextDF c = firstChild c `mplus` up c
  where up x = right x `mplus` (up =<< parent x)

-- | Perform a depth first search for a descendant that satisfies the
-- given predicate.
findRec :: (Cursor tag text -> Bool) -> Cursor tag text -> Maybe (Cursor tag text)
findRec p c = if p c then Just c else findRec p =<< nextDF c


-- | The child with the given index (starting from 0).
getChild :: Int -> Cursor tag text -> Maybe (Cursor tag text)
getChild n loc =
  do (ts,ps) <- downParents loc
     (ls,t,rs) <- splitChildren ts n
     return Cur { current = t, lefts = ls, rights = rs, parents = ps }


-- | private: computes the parent for "down" operations.
downParents :: Cursor tag text -> Maybe ([Node tag text], Path tag text)
downParents loc =
  case current loc of
    Element n a c -> Just ( c
                      , (lefts loc, Tag n a, rights loc) : parents loc
                      )
    _      -> Nothing

getTag :: Node tag text -> Tag tag text
getTag e = Tag { tagName = eName e
               , tagAttribs = eAttributes e
               }


-- Conversions -----------------------------------------------------------------

-- | A cursor for the given content.
fromTree :: Node tag text -> Cursor tag text
fromTree t = Cur { current = t, lefts = [], rights = [], parents = [] }

-- | The location of the first tree in a forest.
fromForest :: [Node tag text] -> Maybe (Cursor tag text)
fromForest (t:ts) = Just Cur { current = t, lefts = [], rights = ts
                                                      , parents = [] }
fromForest []     = Nothing

-- | Computes the tree containing this location.
toTree :: Cursor tag text -> Node tag text
toTree loc = current (root loc)

-- | Computes the forest containing this location.
toForest :: Cursor tag text -> [Node tag text]
toForest loc = let r = root loc in combChildren (lefts r) (current r) (rights r)


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the document?
isRoot :: Cursor tag text -> Bool
isRoot loc = null (parents loc)

-- | Are we at the left end of the the document?
isFirst :: Cursor tag text -> Bool
isFirst loc = null (lefts loc)

-- | Are we at the right end of the document?
isLast :: Cursor tag text -> Bool
isLast loc = null (rights loc)

-- | Are we at the bottom of the document?
isLeaf :: Cursor tag text -> Bool
isLeaf loc = isNothing (downParents loc)

-- | Do we have a parent?
isChild :: Cursor tag text -> Bool
isChild loc = not (isRoot loc)

-- | Get the node index inside the sequence of children
getNodeIndex :: Cursor tag text -> Int
getNodeIndex loc = length (lefts loc)

-- | Do we have children?
hasChildren :: Cursor tag text -> Bool
hasChildren loc = not (isLeaf loc)



-- Updates ---------------------------------------------------------------------

-- | Change the current content.
setContent :: Node tag text -> Cursor tag text -> Cursor tag text
setContent t loc = loc { current = t }

-- | Modify the current content.
modifyContent :: (Node tag text -> Node tag text) -> Cursor tag text -> Cursor tag text
modifyContent f loc = setContent (f (current loc)) loc

-- | Modify the current content.
modifyContentList :: (Node tag text -> [Node tag text]) -> Cursor tag text -> Maybe (Cursor tag text)
modifyContentList f loc = removeGoRight $ insertManyRight (f $ current loc) loc

-- | Modify the current content, allowing for an effect.
modifyContentM :: Monad m => (Node tag text -> m (Node tag text)) -> Cursor tag text -> m (Cursor tag text)
modifyContentM f loc = do x <- f (current loc)
                          return (setContent x loc)

-- | Insert content to the left of the current position.
insertLeft :: Node tag text -> Cursor tag text -> Cursor tag text
insertLeft t loc = loc { lefts = t : lefts loc }

-- | Insert content to the right of the current position.
insertRight :: Node tag text -> Cursor tag text -> Cursor tag text
insertRight t loc = loc { rights = t : rights loc }

-- | Insert content to the left of the current position.
insertManyLeft :: [Node tag text] -> Cursor tag text -> Cursor tag text
insertManyLeft t loc = loc { lefts = reverse t ++ lefts loc }

-- | Insert content to the right of the current position.
insertManyRight :: [Node tag text] -> Cursor tag text -> Cursor tag text
insertManyRight t loc = loc { rights = t ++ rights loc }

-- | Insert content as the first child of the current position.
mapChildren :: ([Node tag text] -> [Node tag text])
            -> Cursor tag text
            -> Maybe (Cursor tag text)
mapChildren f loc = let e = current loc in
  case e of
    Text _ -> Nothing
    Element _ _ c -> Just $ loc { current = e { eChildren = f c } }

-- | Insert content as the first child of the current position.
insertFirstChild :: Node tag text -> Cursor tag text -> Maybe (Cursor tag text)
insertFirstChild t = mapChildren (t:)

-- | Insert content as the first child of the current position.
insertLastChild :: Node tag text -> Cursor tag text -> Maybe (Cursor tag text)
insertLastChild t = mapChildren (++[t])

-- | Insert content as the first child of the current position.
insertManyFirstChild :: [Node tag text] -> Cursor tag text -> Maybe (Cursor tag text)
insertManyFirstChild t = mapChildren (t++)

-- | Insert content as the first child of the current position.
insertManyLastChild :: [Node tag text] -> Cursor tag text -> Maybe (Cursor tag text)
insertManyLastChild t = mapChildren (++t)

-- | Remove the content on the left of the current position, if any.
removeLeft :: Cursor tag text -> Maybe (Node tag text,Cursor tag text)
removeLeft loc = case lefts loc of
                   l : ls -> return (l,loc { lefts = ls })
                   [] -> Nothing

-- | Remove the content on the right of the current position, if any.
removeRight :: Cursor tag text -> Maybe (Node tag text,Cursor tag text)
removeRight loc = case rights loc of
                    l : ls -> return (l,loc { rights = ls })
                    [] -> Nothing


-- | Insert content to the left of the current position.
-- The new content becomes the current position.
insertGoLeft :: Node tag text -> Cursor tag text -> Cursor tag text
insertGoLeft t loc = loc { current = t, rights = current loc : rights loc }

-- | Insert content to the right of the current position.
-- The new content becomes the current position.
insertGoRight :: Node tag text -> Cursor tag text -> Cursor tag text
insertGoRight t loc = loc { current = t, lefts = current loc : lefts loc }

-- | Remove the current element.
-- The new position is the one on the left.
removeGoLeft :: Cursor tag text -> Maybe (Cursor tag text)
removeGoLeft loc = case lefts loc of
                     l : ls -> Just loc { current = l, lefts = ls }
                     []     -> Nothing

-- | Remove the current element.
-- The new position is the one on the right.
removeGoRight :: Cursor tag text -> Maybe (Cursor tag text)
removeGoRight loc = case rights loc of
                     l : ls -> Just loc { current = l, rights = ls }
                     []     -> Nothing

-- | Remove the current element.
-- The new position is the parent of the old position.
removeGoUp :: Cursor tag text -> Maybe (Cursor tag text)
removeGoUp loc =
  case parents loc of
    (pls,v,prs) : ps -> Just
      Cur { current = fromTag v (reverse (lefts loc) ++ rights loc)
          , lefts = pls, rights = prs, parents = ps
          }
    [] -> Nothing


-- | private: Gets the given element of a list.
-- Also returns the preceding elements (reversed) and the following elements.
splitChildren :: [a] -> Int -> Maybe ([a],a,[a])
splitChildren _ n | n < 0 = Nothing
splitChildren cs pos = loop [] cs pos
  where loop acc (x:xs) 0 = Just (acc,x,xs)
        loop acc (x:xs) n = loop (x:acc) xs $! n-1
        loop _ _ _        = Nothing

-- | private: combChildren ls x ys = reverse ls ++ [x] ++ ys
combChildren :: [a] -> a -> [a] -> [a]
combChildren ls t rs = foldl (flip (:)) (t:rs) ls

