arraydif :: [Int] -> [Int] -> [Int]
arraydif [] [] = []
arraydif (a:as) (b:bs) = (a - b) : arraydif as bs

-- Alternatives
    -- List Comprehension with !! (Index operator) or Zip
    -- Zipwith function