{-#LANGUAGE NamedFieldPuns #-}
module PBook () where 

import Network.Browser
import Data.Maybe (fromJust)
import Network.HTTP.Base (RequestMethod(POST))
import Network.URI (parseURI)
import Data.Random (runRVar)
import Data.Random.Source.DevRandom
import Data.Random.Extras (choicesArray)
import Data.Array (listArray)
import Data.List (intercalate)
import Control.Monad (liftM)
import Data.Monoid (mconcat)

type Prediction = String
type Chance = String -- Should be constrained to be an integer percentage of no more than 3 digits.
type Date = String 
type AuthenticityToken = String
type UUID = String

main = do
  p <- promptPrediction 
  c <- promptChance 
  d <- promptDate
  t <- getAuthenticityToken
  u <- genUUID
  postPrediction p c d t u

getAuthenticityToken :: IO AuthenticityToken --Turns out this value can be mangled without affecting the ability to post predictions. 
getAuthenticityToken = return "r78doqWWedU/2ElkBctqlUlb7lDzZVgEY0yXkf2fnlw="

genUUID :: IO UUID --This is sometimes extremely slow. Why?
genUUID = liftM (intercalate "-") $ mapM hexes [8,4,4,4,12]
  where hexes n  = runRVar (choicesArray n hexArray) DevRandom
	hexArray = listArray (0::Int,15::Int) "0123456789abcdef"

postPrediction :: Prediction -> Chance -> Date -> AuthenticityToken -> UUID -> IO ()
postPrediction p c d t u = do
  auth <- promptAuth
  res <- browse $ do
    setAllowBasicAuth True
    addAuthority auth 
    request $ formToRequest predictionForm
  print res
  where predictionForm = Form POST 
	  (fromJust $ parseURI "http://predictionbook.com/predictions/") 
	  [("utf8","✓"), 
	   ("authenticity_token",t), 
	   ("prediction[uuid]",u),
	   ("prediction[description]",p), 
	   ("prediction[initial_confidence]",c), 
	   ("prediction[deadline_text]",d)]

promptAuth :: IO Authority
promptAuth = do
  putStrLn "Username?"
  auUsername <- getLine
  putStrLn "Password?"
  auPassword <- getLine
  return AuthBasic { auRealm,  auUsername, auPassword, auSite}
  where auSite  = fromJust $ parseURI "http://predictionbook.com" 
	auRealm = "predictionbook.com"

promptPrediction :: IO Prediction
promptPrediction = putStrLn "What do you think will (or won't) happen?" >> getLine

promptChance :: IO Chance
promptChance = putStrLn "What's your estimate of this happening (as an integer percentage)?" >> getLine

promptDate :: IO Date
promptDate = putStrLn "When will you know?" >> getLine

