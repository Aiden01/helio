{-# LANGUAGE LambdaCase #-}

module Helio.Args
  (
  )
where

import Helio.Common

import Control.Lens
import Data.List (intercalate)


getArgs :: BotM (Maybe Args)
getArgs = use args >>= \case
  [] -> pure Nothing
  as -> pure (Just as)

getFirst :: BotM (Maybe String)
getFirst = head <$$> getArgs <* advance

single :: Read a => BotM (Maybe a)
single = read <$$> getFirst

rest :: BotM (Maybe String)
rest = intercalate " " <$$> getArgs

advance :: BotM ()
advance = args %= tail
