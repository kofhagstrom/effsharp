module Expects where

import Parser (Parser (..))
import ResultHelper (unwrapError, unwrapOk)

ok :: (Show a, Show error) => Parser a error ok -> a -> ok
ok f input = unwrapOk $ snd <$> run f input

error :: (Show a, Show output) => Parser a b output -> a -> b
error f input = snd $ unwrapError (run f input)