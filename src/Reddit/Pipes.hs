module Reddit.Pipes
  ( commentStream
  , postStream
  , redditStream
  , allUserComments ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Pipes (Producer)
import Reddit
import Reddit.Types.Comment
import Reddit.Types.Post
import Reddit.Types.Listing hiding (before, after)
import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User
import qualified Pipes

type Interval = Int

redditStream :: MonadIO m => Maybe SubredditName -> Maybe a -> Interval -> (b -> a) -> (Options a -> Maybe SubredditName -> RedditT m (Listing c b)) -> Producer b (RedditT m) ()
redditStream sub pid interval f g = do
  Listing _ _ xs <- Pipes.lift $ g (Options (Before <$> pid) (Just 100)) sub
  mapM_ Pipes.yield xs
  case xs of
    [] -> do
      liftIO $ threadDelay $ interval * 1000 * 1000
      redditStream sub pid interval f g
    y : _ -> do
      redditStream sub (Just $ f y) interval f g

postStream :: MonadIO m => Maybe SubredditName -> Interval -> Producer Post (RedditT m) ()
postStream sub interval =
  redditStream sub Nothing interval postID (\o s -> getPosts' o New s)

commentStream :: MonadIO m => Maybe SubredditName -> Interval -> Producer Comment (RedditT m) ()
commentStream sub interval =
  redditStream sub Nothing interval commentID getNewComments'

allUserComments :: MonadIO m => Username -> Producer Comment (RedditT m) ()
allUserComments username = go (Just Nothing)
  where
    go Nothing = return ()
    go (Just x) = do
      Listing _ a cs <- Pipes.lift $ getUserComments' (Options x (Just 100)) username
      mapM_ Pipes.yield cs
      go $ Just <$> After <$> a
