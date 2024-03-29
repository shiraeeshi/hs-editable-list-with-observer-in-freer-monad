{-# LANGUAGE GADTs #-}
{-# LANGUAGE  RankNTypes #-}
module FreerProgram (Program(..), foldFreer) where

data Program instr a where
  Done :: a -> Program instr a
  Bind :: Program instr a -> (a -> Program instr b) -> Program instr b
  Instr :: instr a -> Program instr a

instance Functor (Program instr) where
  fmap f x = x `Bind` (\x' -> Done (f x'))

instance Applicative (Program instr) where
  pure = Done
  f <*> x = f `Bind` (\f' -> x `Bind` (\x' -> Done (f' x')))

instance Monad (Program instr) where
  return = Done
  (>>=) = Bind

foldFreer :: Monad m => (forall r. instr r -> m r) -> Program instr a -> m a
foldFreer _ (Done x) = return x
foldFreer interpret (x `Bind` f) = do x' <- foldFreer interpret x
                                      foldFreer interpret (f x')
foldFreer interpret (Instr x) = interpret x
