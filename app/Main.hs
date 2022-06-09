module Main where

import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as HS
import qualified Noaa as NOAA


main :: IO ()
main = do
  response <- HS.httpLBS NOAA.request
  let status = HS.getResponseStatusCode response
  if status == 200
  then do
    print "saving request to file"
    let jsonBody = HS.getResponseBody response
    L.writeFile "data.json" jsonBody
  else print "request failed with error"

  jsonData <- L.readFile "data.json"
  let noaaResponse = A.decode jsonData :: Maybe NOAA.Response
  let noaaResults = NOAA.results <$> noaaResponse
  NOAA.printResults noaaResults
