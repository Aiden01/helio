{-# LANGUAGE TemplateHaskell #-}

module Helio.Bot
  ( setToken
  )
where

import Helio.Common
import Control.Lens

setToken :: String -> BotM ()
setToken = ((config . token) .=)







