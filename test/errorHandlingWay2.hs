import Text.XML.Expat.Tree
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
import Control.Exception.Extensible as E

-- This is not the recommended way to handle errors. See errorHandlingWay1.hs
main = do
    do
        let tree = parseTreeThrowing Nothing (L.pack $ map c2w $ "<top><banana></apple></top>")
        print (tree :: UNode String)
        -- Because of lazy evaluation, you should not process the tree outside the 'do' block,
        -- or exceptions could be thrown that won't get caught.
    `E.catch` (\exc ->
        case E.fromException exc of
            Just (XMLParseException err) -> putStrLn $ "It failed : "++show err
            Nothing -> E.throwIO exc)
