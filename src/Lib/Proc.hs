module Lib.Proc
    ( sysOut
    , sysEitherT)
where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Either (EitherT)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Exit (ExitCode(..))
import           System.Process.Extra (systemOutput)

sysOut :: (MonadIO m) => FilePath -> [Text] -> m (ExitCode, Text)
sysOut path args = do
    let args' = [path] <> map Text.unpack args
        cmd   = unwords $ map wrap args'
    (ret, out) <- liftIO $ systemOutput cmd
    return (ret, Text.pack out)
  where
      wrap :: String -> String
      wrap s = "'" <> s <> "'"

sysEitherT
    :: (MonadIO m)
    => IO (ExitCode, Text)
    -> (Text -> EitherT e m a)
    -> (Text -> EitherT e m a)
    -> EitherT e m a
sysEitherT action left_ right_ = do
    (ret, out) <- liftIO action
    case ret of
        ExitSuccess   -> right_ out
        ExitFailure _ -> left_ out
