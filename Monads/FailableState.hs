module FailableState where

import Prelude hiding (fail)
import Control.Monad (when)


newtype FailableState a x = FailableState { runFailableState :: a -> Maybe (x, a)}

get :: FailableState a a
get = FailableState (\ a -> Just (a, a))

put :: a -> FailableState a ()
put a = FailableState (const (Just ((), a)))


fail :: FailableState a x
fail = FailableState (const Nothing)

instance Functor (FailableState s) where
  fmap f ma = FailableState (\ s -> case runFailableState ma s of
                                 Nothing -> Nothing
                                 Just (a, s') -> Just (f a, s')
                       )

instance Applicative (FailableState s) where
  (<*>) mf mx = FailableState (\ s -> case runFailableState mf s of
                                 Nothing -> Nothing
                                 Just (f, s') -> case runFailableState mx s' of
                                                   Nothing -> Nothing
                                                   Just (x, s'') -> Just (f x, s'')
                            )

  pure x = FailableState (\ s -> Just (x, s))


instance Monad (FailableState s) where
  (>>=) ma f = FailableState (\ s -> case runFailableState ma s of
                                 Nothing -> Nothing
                                 (Just (a, s')) -> runFailableState (f a) s'
                       )



modifyFS :: (a -> a) -> FailableState a ()
modifyFS f = do
  s <- get
  put (f s)



fsFib :: FailableState Int (Int, Int,Int)
fsFib = do
  s <- get
  if s < 0
    then fail
    else  case s of
    0 -> pure (0, 0, 0)
    1 -> pure (0, 0, 1)
    _ -> do
      modifyFS pred
      (_, b, c) <- fsFib
      pure (b, c, b + c)

fsFib10 = runFailableState fsFib 10


fsFibN = runFailableState fsFib

reader :: FailableState String String
reader = do
  empty <- fmap null get
  if empty
    then pure []
    else do c <- fmap head get
            modifyFS tail
            when (not $ elem c "abcd") fail
            res <- reader
            pure $ c : res
