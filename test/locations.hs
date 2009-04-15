-- | Parse to sax events with locations

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
            hPutStrLn stderr "Usage: locations <file.xml>"
            exitWith $ ExitFailure 1

process :: String -> IO ()
process filename = do
    inputText <- L.readFile filename
    -- Note: Because we're not using the tree, Haskell can't infer the type of
    -- strings we're using so we need to tell it explicitly with a type signature.
    let events = parseSAXLocations Nothing inputText :: [(SAXEvent String String, XMLParseLocation)]
    mapM_ print events

