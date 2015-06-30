module Reddit.Pipes
  ( commentStream
  , postStream
  , redditStream
  , allUserComments ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Function
import Pipes (Producer)
import Prelude
import Reddit
import Reddit.Types.Comment
import Reddit.Types.Listing hiding (before, after)
import Reddit.Types.Options
import Reddit.Types.Post
import Reddit.Types.Subreddit
import Reddit.Types.User
import qualified Pipes
import qualified Data.BoundedSet as Bounded

type Interval = Int

redditStream :: (Ord a, MonadIO m) => Maybe SubredditName -> Interval -> (b -> a) -> (Options a -> Maybe SubredditName -> RedditT m (Listing c b)) -> Producer b (RedditT m) ()
redditStream sub interval f g = flip fix (Bounded.empty 200) $ \loop s -> do
  liftIO $ print $ Bounded.size s
  Listing _ _ xs <- Pipes.lift $ g (Options Nothing (Just 100)) sub
  let news = filter (\x -> not $ Bounded.member (f x) s) xs
  mapM_ Pipes.yield news
  liftIO $ threadDelay $ interval * 1000 * 1000
  loop (Bounded.insertAll (map f news) s)

postStream :: MonadIO m => Maybe SubredditName -> Interval -> Producer Post (RedditT m) ()
postStream sub interval =
  redditStream sub interval postID (\o s -> getPosts' o New s)

commentStream :: MonadIO m => Maybe SubredditName -> Interval -> Producer Comment (RedditT m) ()
commentStream sub interval =
  redditStream sub interval commentID getNewComments'

allUserComments :: MonadIO m => Username -> Producer Comment (RedditT m) ()
allUserComments username = go (Just Nothing)
  where
    go Nothing = return ()
    go (Just x) = do
      Listing _ a cs <- Pipes.lift $ getUserComments' (Options x (Just 100)) username
      mapM_ Pipes.yield cs
      go $ Just <$> After <$> a
