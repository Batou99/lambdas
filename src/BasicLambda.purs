module BasicLambda where

import Prelude (class Show, Unit, discard, pure, show, unit, ($))
import Control.Monad.Except
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)
import Data.Either (Either(..))

import AWS.Lambda.Context (LAMBDA, Context, succeed, fail)
import BasicData (readLambdaData)

handler :: forall eff. Context -> Foreign -> Eff (lambda :: LAMBDA | eff) Unit
handler c d = do
  process $ runExcept (readLambdaData d)
  pure unit

  where
    process :: forall e a. Show e => Show a => Either e a -> Eff (lambda :: LAMBDA | eff) Unit
    process (Left err) = fail c $ show err
    process (Right d')  = succeed c $ show d'
