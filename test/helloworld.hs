-- | A "hello world" example of hexpat that lazily parses a document, printing
-- it to standard out.

import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy as L

main = do
    args <- getArgs
    case args of
        [filename] -> process filename
        otherwise  -> do
            hPutStrLn stderr "Usage: helloworld <file.xml>"
            exitWith $ ExitFailure 1

process :: String -> IO ()
process filename = do
    inputText <- L.readFile filename
    -- Note: Because we're not using the tree, Haskell can't infer the type of
    -- strings we're using so we need to tell it explicitly with a type signature.
    let (xml, mErr) = parseTree Nothing inputText :: (UNode String, Maybe XMLParseError)
    -- Process document before handling error, so we get lazy processing.
    L.hPutStr stdout $ formatTree xml
    putStrLn ""
    case mErr of
        Nothing -> return ()
        Just err -> do
            hPutStrLn stderr $ "XML parse failed: "++show err
            exitWith $ ExitFailure 2

