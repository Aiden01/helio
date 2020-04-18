
{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

module Helio.Common
  ( BotM
  , Color(..)
  , CommandInfo(..)
  , ownerOnly
  , args
  , name
  , description
  , neededRoles
  , neededPermissions
  , Handler
  , Command(..)
  , BotConfig(..)
  , embedColor
  , token
  , ownerIds
  , deleteMessages
  , prefix
  , BotState(..)
  , config
  , commands
  , Args
  , (<$$>)
  , message
  , conn
  , (.:)
  , Hook(..)
  , hooks
  , Parser
  , argsMax
  , argsMin
  , argsCount
  )
where

import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import qualified Data.Text as T
import Discord.Internal.Types (Snowflake)
import Discord.Internal.Types.Channel (Message)
import Discord (DiscordHandle)
import qualified Data.Map as M
import Data.Void (Void)
import Text.Megaparsec (ParsecT)

type Parser = ParsecT Void T.Text BotM

data Color = Color Int Int Int
  deriving Show

type Args = [String]

data CommandInfo = CommandInfo
  { _ownerOnly :: Bool
  , _argsMax :: Maybe Int
  , _argsMin :: Maybe Int
  , _argsCount :: Maybe Int
  , _name :: String
  , _description :: Maybe String
  , _neededRoles :: [String]
  , _neededPermissions :: [String]
  , _hooks :: [Hook] }

type Handler = [String] -> BotM T.Text
data Command = Command CommandInfo Handler

data Hook 
  = BeforeMessage (BotM ())
  | AfterMessage (BotM ())
  | Firewall (BotM Bool)

type BotM = StateT BotState (ExceptT String IO)


data BotConfig = BotConfig
  { _embedColor :: Color
  , _token :: String
  , _ownerIds :: [Snowflake]
  , _deleteMessages :: Bool
  , _prefix :: String }

data BotState = BotState
  { _config :: BotConfig
  , _commands :: M.Map String Command
  , _args :: Args
  , _message :: Message
  , _conn :: DiscordHandle }

makeLenses ''BotConfig
makeLenses ''BotState
makeLenses ''CommandInfo

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \ a b -> f (g a b)




