module Reddit.Pipes
  ( commentStream
  , postStream
  , allUserComments ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Foldable (foldrM)
import Pipes (Producer)
import Reddit
import Reddit.Types.Comment
import Reddit.Types.Post
import Reddit.Types.Listing hiding (before, after)
import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User
import qualified Data.Set as Set
import qualified Pipes

type Interval = Int

type Limit = Int

commentStream :: MonadIO m => Interval -> Maybe Limit -> Maybe SubredditName -> Producer Comment (RedditT m) ()
commentStream = redditStream getNewComments' commentID

postStream :: MonadIO m => Interval -> Maybe Limit -> Maybe SubredditName -> Producer Post (RedditT m) ()
postStream = redditStream (\opts r -> getPosts' opts New r) postID

redditStream :: (MonadIO m, Ord o) => (Options a -> Maybe SubredditName -> RedditT m (Listing a r)) -> (r -> o) -> Int -> Maybe Limit -> Maybe SubredditName -> Producer r (RedditT m) ()
redditStream f g interval opts r = go Set.empty
  where
    go set = do
      Listing _ _ cs <- Pipes.lift $ f (Options Nothing opts) r
      set' <- foldrM handle set cs
      liftIO $ threadDelay $ interval * 1000 * 1000
      go set'
    handle comment set =
      if Set.member (g comment) set
        then return set
        else do
          Pipes.yield comment
          return $ Set.insert (g comment) set

allUserComments :: MonadIO m => Username -> Producer Comment (RedditT m) ()
allUserComments username = go (Just Nothing)
  where
    go Nothing = return ()
    go (Just x) = do
      Listing _ a cs <- Pipes.lift $ getUserComments' (Options x (Just 100)) username
      mapM_ Pipes.yield cs
      go $ Just <$> After <$> a
