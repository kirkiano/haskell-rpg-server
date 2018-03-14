{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.Listen.Auth.LDAP ( authLDAP ) where

import RPGServer.Common
import Control.Monad.IO.Class        ( liftIO,
                                       MonadIO )
import System.Exit                   ( ExitCode(ExitSuccess) )
import qualified Data.Text           as T
import qualified System.Process      as P
import RPGServer.World               ( CharacterID )
import RPGServer.Listen.Auth.Message ( Credentials(..) )


authLDAP :: MonadIO m => Credentials -> m (Maybe CharacterID)
authLDAP c = return Nothing {- do -- TODO: actually use LDAP auth
  let cmd0 = "LDAPTLS_REQCERT=never "
      cmd1 = "ldapwhoami -H ldaps://ldap:835 -D \"uid="
      cmd2 = ",ou=users,dc=example,dc=com\" -W"
      cmd  = cmd0 ++ cmd1 ++ (T.unpack $ user c) ++ cmd2
  (ec, _, _) <- liftIO $
                P.readCreateProcessWithExitCode
                (P.shell cmd)
                (T.unpack $ pass c)
  return $ case ec of
    ExitSuccess -> Nothing -- TODO: actually use LDAP auth
    _           -> Nothing
-}
