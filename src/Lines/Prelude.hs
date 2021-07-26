module Lines.Prelude
    ( module X
    , module Lines.Prelude
    ) where

import           RIO      as X hiding (exitSuccess)

import           RIO.Char as X (isSpace)
import           RIO.List as X (dropWhileEnd, find, genericLength, headMaybe,
                                maximumByMaybe, maximumMaybe, minimumByMaybe,
                                minimumMaybe)
import           RIO.Text as X (pack, unpack)

import qualified RIO.Text as T

withRIO :: (env' -> env) -> RIO env a -> RIO env' a
withRIO f m = do
    env <- asks f
    runRIO env m

guardM :: (Monad m, Alternative m) => m Bool -> m ()
guardM mb = do
    b <- mb
    guard b

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8With T.lenientDecode

handles :: MonadUnliftIO m => [Handler m a] -> m a -> m a
handles = flip catches
