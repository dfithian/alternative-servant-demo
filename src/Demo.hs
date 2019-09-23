module Demo where

import ClassyPrelude
import Data.Aeson (FromJSON, parseJSON)
import Data.Functor.Alt ((<!>))
import Data.Proxy (Proxy (Proxy))
import Servant ((:<|>) ((:<|>)), (:>), Get, GetNoContent, JSON, NoContent, QueryParam)
import Servant.Client (ClientEnv, ClientM, client, runClientM)
import Servant.Client.Core (ClientError)

data Baz = Baz

instance FromJSON Baz where
  parseJSON _ = pure Baz

type BazApi = "foo" :> "bar" :> QueryParam "page" Int :> (Get '[JSON] [Baz] :<|> GetNoContent '[] NoContent)

getBaz :: Maybe Int -> ClientM [Baz] :<|> ClientM NoContent
getBaz = client (Proxy @BazApi)

runGetBaz :: (MonadIO m) => ClientEnv -> Maybe Int -> m (Either ClientError [Baz])
runGetBaz env page = liftIO . flip runClientM env $ do
  -- pattern match on the possible results and use the one that is successful via Alt (<!>)
  case getBaz page of
    someRecords :<|> noRecords -> someRecords <!> ([] <$ noRecords)
