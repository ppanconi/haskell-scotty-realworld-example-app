{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Common.EnsureUserCredsUnique where

import Conduit.DB.Core ( mapDBResult, MonadDB(..) )
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (User)
import Conduit.Features.Account.Errors (AccountError(..))
import Database.Esqueleto.Experimental (Value(..), exists, from, selectOne, table, val, (==.))
import UnliftIO (MonadUnliftIO)

type Name = Maybe Text
type Mail = Maybe Text

ensureUserCredsUnique :: (ReadUsers m) => Name -> Mail -> m (Either AccountError ())
ensureUserCredsUnique name email = runExceptT do
  errs <- ExceptT $ findDuplicateCreds name email

  ExceptT . pure $ case errs of
    [] -> pure ()
    cs -> Left $ CredsTaken cs

class (Monad m) => ReadUsers m where
  findDuplicateCreds :: Name -> Mail -> m (Either AccountError [(Text, Text)])

instance (Monad m, MonadUnliftIO m, MonadDB m) => ReadUsers m where
  findDuplicateCreds :: Name -> Mail -> m (Either AccountError [(Text, Text)])
  findDuplicateCreds name email = mapDBResult processResult <$> runDB do
    
    res <- selectOne $ do
      let nameExists = checkExists name \n ->
            exists $ void $ from (table @User)
              `suchThat` \u -> u.username ==. val n

      let mailExists = checkExists email \m ->
            exists $ void $ from (table @User)
              `suchThat` \u -> u.email ==. val m
      pure (nameExists, mailExists)
    case res of
      Just (nE, eE) -> return $ Just ((nE, fromMaybe "" name), (eE, fromMaybe "" email))
      Nothing -> return Nothing
    where
      checkExists = flip $ maybe (val False)

processResult :: Maybe ((Value Bool, Text), (Value Bool, Text)) -> [(Text, Text)]
processResult (Just ((Value nameExists, nameValue), (Value mailExists, mailValue))) = map snd $ filter fst [(nameExists, ("username", nameValue)), (mailExists, ("email", mailValue))]
processResult Nothing = []
