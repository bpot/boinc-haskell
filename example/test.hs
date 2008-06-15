module Main where
import Boinc.API

divisors :: Int -> [Int]
divisors n = [x | x <- [2..(floor (sqrt n))], mod n x == 0]

isPrime :: Int -> Bool
isPrime n = null (divisors n)

atoi :: String -> Int
atoi s = read s

lineFor :: Int -> String
lineFor n = if isPrime n
              then (show n) ++ " - Prime"
              else (show n) ++ " - Not Prime"

genOutput :: [Int] -> String
genOutput ns = unlines (map lineFor ns)

main = do
  Boinc.API.boincInit
  infile <- Boinc.API.boincResolveFilename "in"
  outfile <- Boinc.API.boincResolveFilename "out"
  indata <- readFile infile
  writeFile outfile (genOutput (map atoi (lines indata)))
  Boinc.API.boincFinish 0
  return 0
