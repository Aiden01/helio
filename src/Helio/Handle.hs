{-# LANGUAGE LambdaCase #-}

module Helio.Handle
  ( sendMessage
  , deleteMessage
  , getMembers
  )
where

import Control.Monad.Trans (liftIO)
import Control.Monad.Except (throwError)
import Control.Lens (use)
import qualified Discord.Requests as R
import Discord.Internal.Rest (Request)
import Discord.Internal.Types
import Discord (restCall, FromJSON, DiscordHandle)
import qualified Data.Text as T

import Helio.Common

makeRequest :: (FromJSON a, Request (r a)) => r a -> BotM a
makeRequest r = use conn
  >>= liftIO . flip restCall r
  >>= either (throwError . show) pure

--- Messages

sendMessage :: ChannelId -> T.Text -> BotM Message
sendMessage = makeRequest .: R.CreateMessage

deleteMessage :: ChannelId -> MessageId -> BotM ()
deleteMessage = makeRequest .: curry R.DeleteMessage 

-- Guild

getMembers :: GuildId -> BotM [GuildMember]
getMembers = makeRequest . flip R.ListGuildMembers (R.GuildMembersTiming Nothing Nothing)
