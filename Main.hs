module PBook where
import Network.HTTP

type Prediction = String
type Chance = String -- Constrained to be an integer percentage of no more than 3 digits.
type Date = String 
type AuthenticityToken = String
type UUID = String

main = do
  p <- promptPrediction 
  c <- promptChance 
  d <- promptDate
  t <- getAuthenticityToken
  u <- getPredictionUUID
  postPrediction p c d t u

getAuthenticityToken :: IO AuthenticityToken
getAuthenticityToken = return "r78doqWWedU/2ElkBctqlUlb7lDzZVgEY0yXkf2fnlw="

getPredictionUUID :: IO UUID
getPredictionUUID = return "1073f8d7-3600-4722-bd44-1090a887a0aa"

postPrediction :: Prediction -> Chance -> Date -> AuthenticityToken -> UUID -> IO ()
postPrediction p c d t u = do
  let req = postRequestWithBody "http://predictionbook.com/predictions/" "application/x-www-form-urlencoded" ("utf8=✓&authenticity_token=" ++ t ++ "&prediction[uuid]=" ++ u ++ "&prediction[description]=" ++ p ++ "&prediction[initial_confidence]=" ++ c ++ "&prediction[deadline_text]=" ++ d )
  res <- simpleHTTP req 
  print res

promptPrediction :: IO Prediction
promptPrediction = putStrLn "What do you think will (or won't) happen?" >> getLine

promptChance :: IO Chance
promptChance = putStrLn "What's your estimate of this happening (as a percentage)?" >> getLine

promptDate :: IO Date
promptDate = putStrLn "When will you know?" >> getLine

