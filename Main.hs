module PBook where
import Network.HTTP
import Data.Random
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
  u <- getUUID
  postPrediction p c d t u

getAuthenticityToken :: IO AuthenticityToken
getAuthenticityToken = return "r78doqWWedU/2ElkBctqlUlb7lDzZVgEY0yXkf2fnlw="

getUUID :: IO UUID --This is sometimes extremely slow. Why?
getUUID = liftM (intercalate "-") $ mapM hexes [8,4,4,4,12]
  where hexes n = runRVar (choicesArray n $ listArray (0::Int,15::Int) "0123456789abcdef") DevRandom

postPrediction :: Prediction -> Chance -> Date -> AuthenticityToken -> UUID -> IO ()
postPrediction p c d t u = do
  let params = shows "utf8=✓&authenticity_token=" . shows t . shows "&prediction[uuid]=" . shows u . shows "&prediction[description]=" . shows p . shows "&prediction[initial_confidence]=" . shows c . shows "&prediction[deadline_text]=" $ d
  let req = postRequestWithBody "http://predictionbook.com/predictions/" "application/x-www-form-urlencoded" params 
  res <- simpleHTTP req
  print res

promptPrediction :: IO Prediction
promptPrediction = putStrLn "What do you think will (or won't) happen?" >> getLine

promptChance :: IO Chance
promptChance = putStrLn "What's your estimate of this happening (as a percentage)?" >> getLine

promptDate :: IO Date
promptDate = putStrLn "When will you know?" >> getLine

