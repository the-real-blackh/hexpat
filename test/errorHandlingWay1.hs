import Text.XML.Expat.Tree
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)

-- This is the recommended way to handle errors in lazy parses
main = do
    let (tree, mError) = parseTree Nothing (L.pack $ map c2w $ "<top><banana></apple></top>")
    print (tree :: UNode String)
    -- Note: We check the error _after_ we have finished our processing on the tree.
    case mError of
        Just err -> putStrLn $ "It failed : "++show err
        Nothing -> putStrLn "Success!"
