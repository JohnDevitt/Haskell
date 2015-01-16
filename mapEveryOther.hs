
mapEveryOther function (x:xs) = mapOne function (x:xs)

mapOne function (x:xs) = (function x) : (mapTwo function xs)
mapOne function [] = []

mapTwo function (x:xs) = x : (mapOne function xs)
mapTwo function [] = []