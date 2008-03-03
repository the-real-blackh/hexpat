-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- This program microbenchmarks HaXml versus Expat parsing.
-- The comparision is pretty unfair; HaXml does a whole lot more.
-- However, if you just want to read all the XML tags in a file,
-- this program demonstrates why Expat may be preferable.

import Text.XML.HaXml as HaXml
import Text.XML.Expat.IO as Expat
import Control.Exception
import Data.IORef
import Microbench

parse_haxml :: String -> IO ()
parse_haxml input = do
  let Document _ _ root _ = HaXml.xmlParse "input" input
  let Elem _ _ content = root
  evaluate $ length content
  return ()

parse_expat :: String -> IO ()
parse_expat input = do
  parser <- Expat.newParser Nothing
  counter <- newIORef 0
  Expat.setStartElementHandler parser (elementHandler counter)
  Expat.parse parser input True
  readIORef counter
  return ()
  where
  elementHandler counter tag attrs = modifyIORef counter (+1)

main = do
  xml <- readFile "test.xml"
  -- Force reading the entire file first.
  putStrLn $ "input is " ++ show (length xml) ++ " bytes."
  -- Start the races.
  microbench "HaXml" (parse_haxml xml)
  microbench "hexpat" (parse_expat xml)
