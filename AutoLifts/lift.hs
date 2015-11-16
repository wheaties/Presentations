liftM :: Monad m => (a -> b) -> m a -> m b

(liftM f) . (liftM g) Ma

(liftM f) . (liftM g) == liftM (f . g)

(liftM . liftM) f == liftM (liftM f)

liftM2 (+) [0, 1] [1, 2] == [1, 2, 3, 4]