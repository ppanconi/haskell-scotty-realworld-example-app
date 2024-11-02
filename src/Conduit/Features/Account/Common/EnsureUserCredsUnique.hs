{-# LANGUAGE UndecidableInstances #-}

module Conduit.Features.Account.Common.EnsureUserCredsUnique where

import Conduit.DB.Core ( mapDBResult, MonadDB(..) )
import Conduit.DB.Utils (suchThat)
import Conduit.Features.Account.DB (User)
import Conduit.Features.Account.Errors (AccountError(..))
import Database.Esqueleto.Experimental
    ( Value(..),
      exists,
      from,
      selectOne,
      table,
      val,
      (==.),
      (!=.),
      (&&.),
      valkey )
import UnliftIO (MonadUnliftIO)
import Conduit.Features.Account.Types (UserID(..))

type Name = Maybe Text
type Mail = Maybe Text
type AuthedUserID = Maybe UserID
data MailOrName = Mail | Name
data PropAuthId = PropAuthId
  { prop :: Text
  , authedUserID  :: AuthedUserID
}


ensureUserCredsUnique :: (ReadUsers m) => Name -> Mail -> AuthedUserID -> m (Either AccountError ())
ensureUserCredsUnique name email authedUserID = runExceptT do
  errs <- ExceptT $ findDuplicateCreds name email authedUserID

  ExceptT . pure $ case errs of
    [] -> pure ()
    cs -> Left $ CredsTaken cs

class (Monad m) => ReadUsers m where
  findDuplicateCreds :: Name -> Mail -> AuthedUserID -> m (Either AccountError [(Text, Text)])

instance (Monad m, MonadUnliftIO m, MonadDB m) => ReadUsers m where
  findDuplicateCreds :: Name -> Mail -> AuthedUserID -> m (Either AccountError [(Text, Text)])
  findDuplicateCreds name email authedUserID = mapDBResult processResult <$> runDB do

    res <- selectOne $ do
      let nameExists = checkExists name authedUserID \(PropAuthId n mauthedUserID)  ->
            exists $ void $ from (table @User)
              `suchThat` \u -> u.username ==. val n &&. maybe (val True) (\aid -> u.id !=. valkey aid.unID ) mauthedUserID

      let mailExists = checkExists email authedUserID \(PropAuthId m mauthedUserID) ->
            exists $ void $ from (table @User)
              `suchThat` \u -> u.email ==. val m &&. maybe (val True) (\aid -> u.id !=. valkey aid.unID ) mauthedUserID

      pure (nameExists, mailExists)
    case res of
      Just (nE, eE) -> return $ Just ((nE, fromMaybe "" name), (eE, fromMaybe "" email))
      Nothing -> return Nothing
    where
      checkExists mv uid f =
        case mv of
          Just t -> f (PropAuthId t uid)
          Nothing -> val False

processResult :: Maybe ((Value Bool, Text), (Value Bool, Text)) -> [(Text, Text)]
processResult (Just ((Value nameExists, nameValue), (Value mailExists, mailValue))) = map snd $ filter fst [(nameExists, ("username", nameValue)), (mailExists, ("email", mailValue))]
processResult Nothing = []
