module Meowscript.IO.SafeIO
( SafeIO(..)
) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

class (MonadIO m) => SafeIO m where
    safeIO :: IO (Either Text a) -> m a

    infixl 1 >>|
    (>>|) :: IO (Either Text a) -> (a -> m b) -> m b
    m >>| f = safeIO m >>= f

{- "Why this typeclass?"
 - To make sequencing safe IO actions easier. It's for personal use.
 -
 - "What's a 'safe' IO action?"
 - A 'safe' IO action in my project is an IO action that returns 'Either Text a' instead of
 - throwing an IO exception. This is merely a personal thing.
 -
 - "How is it used?"
 - The (>>|) infix function can be used to sequence safe IO actions into monadic functions without needing
 - to have a nested tree of "case" statements to account for the errors.
 -
 - "Why not define (>>|) as lifting 'IO (Either e a)' into a monad that is an instance of MonadError? Like:"
 -
 -      (>>|) :: (MonadIO m, MonadError e m) => IO (Either e a) -> (a -> m b) -> m b
 -      m >>| f = liftIO m >>= \ma -> case ma of
 -          (Left e)  -> throwError e
 -          (Right a) -> f a
 -
 - ...  For a few reasons:
 -  1. That requires parametrizing the error type, and I didn't want to do that.
 -  2. Because I *want* overriding to be possible. I want the monad to take the Text and turn it into any
 -  wrapped error type it wants. For Meowscript's Evaluator, that is CatException. -}
