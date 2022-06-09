module Noaa
  ( request
  , printResults
  , Response (results)
  ) where


import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.Aeson as A
import qualified GHC.Generics as G
import qualified Control.Monad as M
import qualified Network.HTTP.Simple as HS


data Result = Result { uid :: T.Text
                     , mindate :: T.Text
                     , maxdate :: T.Text
                     , name :: T.Text
                     , datacoverage :: Double
                     , resultId :: T.Text } deriving Show
instance A.FromJSON Result where
  parseJSON (A.Object v) = Result <$> v A..: "uid"
                                  <*> v A..: "mindate"
                                  <*> v A..: "maxdate"
                                  <*> v A..: "name"
                                  <*> v A..:"datacoverage"
                                  <*> v A..: "id"

data Resultset = Resultset { offset :: Int
                           , count :: Int
                           , limit :: Int } deriving (Show,G.Generic)
instance A.FromJSON Resultset

data Metadata = Metadata { resultset :: Resultset } deriving (Show,G.Generic)
instance A.FromJSON Metadata

data Response = Response { metadata :: Metadata
                         , results :: [Result] } deriving (Show,G.Generic)
instance A.FromJSON Response


myToken :: BC.ByteString
myToken = "<APIトークン>"

noaaHost :: BC.ByteString
noaaHost = "www.ncei.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> HS.Request
buildRequest token host method path = HS.setRequestMethod method
                                    $ HS.setRequestHost host
                                    $ HS.setRequestHeader "token" [myToken]
                                    $ HS.setRequestPath path
                                    $ HS.setRequestSecure True
                                    $ HS.setRequestPort 443
                                    $ HS.defaultRequest
request :: HS.Request
request = buildRequest myToken noaaHost "GET" apiPath

printResults :: Maybe [Result] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  M.forM_ results $ \result -> do
    let dataName = name result
    print dataName
