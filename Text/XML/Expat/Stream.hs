module Text.XML.Expat.Stream where

import Control.Exception (bracket)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.XML.Expat.IO as EIO

data Handlers s = Handlers {
  startElementHandler :: Maybe (String -> [(String,String)] -> s -> s),
  endElementHandler   :: Maybe (String -> s -> s),
  charaterDataHandler :: Maybe (String -> s -> s)
}
defaultHandlers = Handlers Nothing Nothing Nothing

parse :: Maybe String -- ^Optional document encoding.
      -> Handlers s
      -> String -- ^Document text.
      -> s -- ^Initial state.
      -> Either () s
parse enc handlers doc st = unsafePerformIO $ setup >>= runParse
  where
  setup = do
    parser <- EIO.newParser enc
    stateref <- newIORef st
    case startElementHandler handlers of
      Just h -> do
        let h' name attrs = modifyIORef stateref (h name attrs)
        EIO.setStartElementHandler parser h'
      Nothing -> return ()
    case endElementHandler handlers of
      Just h -> do
        let h' name = modifyIORef stateref (h name)
        EIO.setEndElementHandler parser h'
      Nothing -> return ()
    return (parser, stateref)
  runParse (parser, stateref) = do
    succ <- EIO.parse parser doc True
    if succ
      then do st <- readIORef stateref; return $ Right st
      else return $ Left ()
