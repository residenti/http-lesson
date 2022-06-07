module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as HS
import qualified GHC.Generics as G
import qualified Control.Monad as M


-- NOTE: NOAA Climate Data APIのデータセットを取得するためのHTTPリクエスト
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

-- TODO: JSONデータをパースする処理は別モジュールに切り出す
data NOAAResult = NOAAResult { uid :: T.Text
                             , mindate :: T.Text
                             , maxdate :: T.Text
                             , name :: T.Text
                             , datacoverage :: Double
                             , resultId :: T.Text } deriving Show

instance A.FromJSON NOAAResult where
  parseJSON (A.Object v) = NOAAResult <$> v A..: "uid"
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

data NOAAResponse = NOAAResponse { metadata :: Metadata
                                 , results :: [NOAAResult] } deriving (Show,G.Generic)

instance A.FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  M.forM_ results $ \result -> do
    let dataName = name result
    print dataName
-- TODO: ここまで

main :: IO ()
main = do
  jsonData <- L.readFile "data.json"
  let noaaResponse = A.decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
  -- response <- HS.httpLBS request
  -- let status = HS.getResponseStatusCode response
  -- if status == 200
  -- then do
  --   print "saving request to file"
  --   let jsonBody = HS.getResponseBody response
  --   L.writeFile "data.json" jsonBody
  -- else print "request failed with error"
