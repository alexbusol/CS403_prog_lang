-- find the length of a list
len [] = 0
len (x:xs) = 1 + len xs

--detect if an element is in the list
detect _ [] = False
detect elem (x:xs) = if elem == x then True else detect elem xs

--append 2 lists
append list1 list2 = list1 ++ list2

--remove an element from the list and return the resulting list
remove _ [] = []
remove elem (x:xs) = if elem == x then remove elem xs else x : remove elem xs

--select  num elements from the list
mytake _ [] = []
mytake 0 _ = []
mytake num (x:xs) = x : mytake (num - 1) xs

--square root implementation using binary search
mysqrt 0 = 0
mysqrt num = sqrt_help num 0 num where
      sqrt_help num low high = let m = (high + low)/2 in 
      if (m * m) == num then m 
      else if (m*m) > num then sqrt_help num low (m-1) 
      else sqrt_help num (m+1) high

--power function
mypower _ 0 = 1
mypower num expt = num * mypower num (expt - 1)

--calculate log of any base
mylog base num = log_help base 1 num where 
    log_help base expt num = if base^expt == num then expt 
    else log_help base (expt + 1) num

--map function
mymap _ [] = []
mymap func (x:xs) = func x : mymap func xs

--use a function to combine elements in the list (basically fold)
combine _ [] = 0
combine func (x:xs) = foldr func x xs
