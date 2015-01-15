
addLnat :: [Int] -> [Int] -> [Int]
addLnat lOne lTwo = addLists 0 lOne lTwo

addLists :: Int -> [Int] -> [Int] -> [Int]
addLists carry (x:xs) (y:ys) = ((x + y + carry) `mod` 10) : addLists (calculateCarry (x+y+carry)) xs ys
addLists carry (x:xs) [] = ((x + carry) `mod` 10) : addLists (calculateCarry (x+carry)) xs []
addLists carry [] (x:xs) = ((x + carry) `mod` 10) : addLists (calculateCarry (x+carry)) [] xs
addLists 1 [] [] = [1]
addLists 0 [] [] = []

mulLnat :: [Int] -> [Int] -> [Int]
mulLnat (x:xs) mulList = addLnat (generateAddtionList 0 x mulList) (0 : (mulLnat xs mulList))
mulLnat [] mulist = []

generateAddtionList :: Int -> Int -> [Int] -> [Int]
generateAddtionList carry x (y:ys) = (((x * y) + carry) `mod` 10) : generateAddtionList (calculateCarry ((x * y) + carry)) x ys
generateAddtionList carry x [] = [carry]

calculateCarry carry = (carry - (carry `mod` 10)) `div` 10