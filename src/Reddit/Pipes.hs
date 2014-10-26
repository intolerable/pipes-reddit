module Reddit.Pipes
  ( commentStream
  , postStream ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Foldable (foldrM)
import Pipes (Producer)
import Reddit.API
import Reddit.API.Types.Comment
import Reddit.API.Types.Post
import Reddit.API.Types.Listing
import Reddit.API.Types.Options
import Reddit.API.Types.Subreddit
import qualified Data.Set as Set
import qualified Pipes

type Interval = Int

type Limit = Int

commentStream :: MonadIO m => Interval -> Maybe Limit -> Maybe SubredditName -> Producer Comment (RedditT m) ()
commentStream = redditStream getNewComments' commentID

postStream :: MonadIO m => Interval -> Maybe Limit -> Maybe SubredditName -> Producer Post (RedditT m) ()
postStream = redditStream (\opts r -> getPosts' opts New r) postID

redditStream ::
  (MonadIO m, Ord ord) => (Options a -> Maybe SubredditName -> RedditT m (Listing a res)) -> (res -> ord) -> Int -> Maybe Limit -> Maybe SubredditName -> Producer res (RedditT m) ()
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
