module PBook where

type Prediction = String
-- Might be interesting to do this as something like: data Chance = Real a => Percent a | Odds a a | Pval a
type Chance = Float
type Date = String

main = do
  p <- promptPrediction 
  c <- promptChance 
  d <- promptDate
  postPrediction p c d

postPrediction :: Prediction -> Chance -> Date -> IO ()
postPrediction p c d = print (p,c,d)

promptPrediction :: IO Prediction
promptPrediction = putStrLn "What do you think will (or won't) happen?" >> getLine

promptChance :: IO Chance
promptChance = putStrLn "What's your estimate of this happening (as a percentage)?" >> readLn

promptDate :: IO Date
promptDate = putStrLn "When will you know?" >> getLine

