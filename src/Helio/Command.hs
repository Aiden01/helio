{-# LANGUAGE TemplateHaskell #-}

module Helio.Command
 ( CmdMod(..)
 , mkCommand
 , withOwnerOnly
 , withDesc
 , withArgsMax
 , withArgsMin
 , withArgsCount
 , mkSimpleCommand
 , withNeededPermissions
 , withNeededRoles
 , withHooks
 , withName
 )
where

import Helio.Common
import Control.Lens

newtype CmdMod = CmdMod (CommandInfo -> CommandInfo)


instance Semigroup CmdMod where
  (CmdMod f) <> (CmdMod g) = CmdMod (f . g)
  

defaultCommand :: String -> CommandInfo
defaultCommand n = CommandInfo
  { _ownerOnly = False
  , _name = n
  , _description = Nothing
  , _neededRoles = []
  , _argsMax = Nothing
  , _argsMin = Nothing
  , _argsCount = Nothing
  , _neededPermissions = []
  , _hooks = [] }

withOwnerOnly :: CmdMod
withOwnerOnly = CmdMod (ownerOnly .~ True)

withDesc :: String -> CmdMod
withDesc desc = CmdMod (description ?~ desc)

withHooks :: [Hook] -> CmdMod
withHooks h = CmdMod (hooks %~ (++ h))

withName :: String -> CmdMod
withName n = CmdMod (name .~ n)

withNeededRoles :: [String] -> CmdMod
withNeededRoles roles = CmdMod (neededRoles %~ (++ roles))

withNeededPermissions :: [String] -> CmdMod
withNeededPermissions perms = CmdMod (neededPermissions %~ (++ perms))

withArgsMax, withArgsMin, withArgsCount :: Int -> CmdMod
withArgsMax n = CmdMod (argsMax ?~ n)
withArgsMin n = CmdMod (argsMin ?~ n)
withArgsCount n = CmdMod (argsCount ?~ n)

mkCommand :: String -> CmdMod -> Handler -> Command
mkCommand n (CmdMod f) handler = Command (f $ defaultCommand n) handler

mkSimpleCommand :: String -> Handler -> Command
mkSimpleCommand n = Command (defaultCommand n)
