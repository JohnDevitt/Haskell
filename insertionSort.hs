
placeIntoSortedList element (x:xs)
	| element > x = x:(placeIntoSortedList (element:xs))
	| element <= x = element:(x:xs)