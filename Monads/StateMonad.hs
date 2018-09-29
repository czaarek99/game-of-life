module StateMonad where

import Control.Monad (when)

newtype MyState a x = MyState { runMyState :: a -> (x, a)}


get :: MyState a a
get = MyState (\ a -> (a,a))

put :: a -> MyState a ()
put a = MyState (const ((), a))

instance Monad (MyState s) where
  (>>=) ma f = MyState (\ s -> let (a, s') = runMyState ma s
                               in runMyState (f a) s'
                       )

modifyState :: (a -> a) -> MyState a ()
modifyState f = do
  s <- get
  put (f s)


stateFunc :: MyState Int String
stateFunc = do
  s <- get
  if s < 0
    then pure "NOTHING"
    else do modifyState (+5)
            s2 <- get
            pure (show s2)

stateFib :: MyState Int (Int, Int,Int)
stateFib = do
  s <- get
  case s of
    0 -> pure (0, 0, 0)
    1 -> pure (0, 0, 1)
    _ -> do
      modifyState pred
      (_, b, c) <- stateFib
      pure (b, c, b + c)

fib10 = runMyState stateFib 10



instance Functor (MyState s) where
  fmap f m = MyState (\ s -> let (x,s') = runMyState m s
                             in  (f x, s'))

instance Applicative (MyState s) where
  (<*>) mf mx = MyState (\ s -> let (f,s') = runMyState mf s
                                    (x,s'') = runMyState mx s'
                                in (f x, s''))
  pure x = MyState (\ a -> (x, a))
