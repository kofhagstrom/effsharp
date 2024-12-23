module ResultHelper (unwrapOk, unwrapError) where

import Result (Result (..))

unwrapOk :: Result err ok -> ok
unwrapOk (Ok ok) = ok
unwrapOk (Error _) = error "Expected ok, got error"

unwrapError :: Result err ok -> err
unwrapError (Error e) = e
unwrapError (Ok _) = error "Expected error, got ok"