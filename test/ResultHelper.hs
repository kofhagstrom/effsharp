module ResultHelper (unwrapOk, unwrapError) where

import Result (Result (..))

unwrapOk :: (Show err) => Result err ok -> ok
unwrapOk (Ok ok) = ok
unwrapOk (Error e) = error $ "Expected ok, got error" ++ show e

unwrapError :: (Show ok) => Result err ok -> err
unwrapError (Error e) = e
unwrapError (Ok a) = error $ "Expected error, got ok" ++ show a