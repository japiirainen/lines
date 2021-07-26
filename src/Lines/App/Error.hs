module Lines.App.Error
    ( AppError(..)
    , mapAppError
    )
    where

import           Lines.Prelude


data AppError
    = GitCloneError Text
    | OtherError SomeException
    | SystemError IOException
    deriving stock Show

instance Exception AppError

mapAppError :: (MonadUnliftIO m, Exception e) => (e -> AppError) -> m a -> m a
mapAppError f = handle $ throwIO . f
