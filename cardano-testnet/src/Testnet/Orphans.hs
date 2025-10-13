{-# OPTIONS_GHC -Wno-orphans #-}

module Testnet.Orphans () where

-- import           Hedgehog (MonadTest(..))
import           RIO (RIO)

-- Not possible to have
-- instance MonadTest (RIO env) where 
--   liftTest  = liftRIO . liftTest
-- If you pattern match on the RIO constructor 
-- it rightfully complains!
-- what extension is responsible for this?
instance MonadFail (RIO env) where 
  fail = error "TODO: throw exception here then catch it in the liftToIntegration "