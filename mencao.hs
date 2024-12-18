mencao :: Float -> String
mencao  n 
    | n >= 9.0 = "SS" 
    | n >= 7.0 = "MS"  
    | n >= 5.0 = "MM" 
    | n > 3.0 = "MI" 
    | otherwise = "SR" 


data Mencao = SR | MI | MM | MS | SS deriving Show

mencao2 :: Float -> Mencao
mencao2 n
    | n >= 9.0 = SS 
    | n >= 7.0 = MS  
    | n >= 5.0 = MM 
    | n > 3.0 = MI 
    | otherwise = SR