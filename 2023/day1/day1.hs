import Data.Char (isDigit, digitToInt)

main = do 
    contents <- readFile "input.txt"
    let linesList = lines contents
    print (getResult linesList)
 
getResult = sum . map ((read :: String -> Int) . getCalibrationValue)

getFirstDigit :: String -> Char 
getFirstDigit (x:xs) = if isDigit x then x else getFirstDigit xs

getLastDigit :: String -> Char 
getLastDigit x = getFirstDigit (reverse x)

getCalibrationValue :: String -> String 
getCalibrationValue x = getFirstDigit x : [getLastDigit x]