
insertionSort [] (x:xs) = insertionSort [x] xs
insertionSort sortedList [] = sortedList
insertionSort sortedList (x:xs) = insertionSort (placeIntoSortedList x sortedList) xs

placeIntoSortedList element [] = [element]
placeIntoSortedList element (x:xs)
	| element > x = x:(placeIntoSortedList element xs)
	| element <= x = element:(x:xs)