-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- This program microbenchmarks HaXml versus Expat parsing.
-- The comparision is pretty unfair; HaXml does a whole lot more.
-- However, if you just want to read all the XML tags in a file,
-- this program demonstrates why Expat may be preferable.

import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Data.Char (ord)
import Data.IORef
import Microbench
import Text.XML.Expat.IO as Expat
import Text.XML.HaXml as HaXml

parse_haxml :: String -> IO ()
parse_haxml input = do
  let Document _ _ root _ = HaXml.xmlParse "input" input
  let Elem _ _ content = root
  evaluate $ length content
  return ()

parse_expat :: BS.ByteString -> IO ()
parse_expat input = do
  parser <- Expat.newParser Nothing
  counter <- newIORef 0
  Expat.setStartElementHandler parser (elementHandler counter)
  Expat.parse parser input
  readIORef counter
  return ()
  where
  elementHandler counter tag attrs = modifyIORef counter (+1)

main = do
  xml <- readFile "test.xml"
  let xmlbs = BS.pack (map (fromIntegral.ord) xml)
  -- Force reading the entire file first.
  putStrLn $ "input is " ++ show (BS.length xmlbs) ++ " bytes."
  -- Start the races.
  microbench "HaXml" (parse_haxml xml)
  microbench "hexpat" (parse_expat xmlbs)
