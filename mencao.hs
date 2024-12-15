mencao :: Float -> String
mencao  n 
    | n >= 9.0 = "SS" 
    | n >= 7.0 = "MS"  
    | n >= 5.0 = "MM" 
    | n > 3.0 = "MI" 
    | otherwise = "SR" 

--main :: IO()
--main = print (mencao 6.9)