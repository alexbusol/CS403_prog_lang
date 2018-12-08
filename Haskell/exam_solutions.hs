convert ([], [], []) = []
convert ((x:xs), (y:ys), (z:zs)) = (x,y,z) : convert (xs, ys, zs)

invert [] = ([], [], [])
invert ((x,y,z):ws) = (x:xs, y:ys, z:zs) where (xs, ys, zs) = invert ws

find v (x:xs) = v==x || v>x && find v xs

splitRange x y zs = ([q | q<-zs, q>=x && q<=y], [q | q<-zs, q<x || q>y])

loop func list 0 = list
loop func list n = loop func (func list) (n-1)

extremes [] = []
extremes ((x:xs):ys) = [x, last xs] : extremes ys

curry3 f x y z = f (x,y,z)
uncurry3 f (x,y,z) = f x y z

count _ [] = 0
count e (x:xs) = if x==e then 1 + count e xs else count e xs

partition p list = ([x | x<-list, p x], [x | x<-list, not (p x)])

isPerfect n = foldr (+) 0 [x | x<-[1..n-1], mod n x == 0] == n

rotations list = map (\k -> drop k list ++ take k list) [0..length list - 1]

transpose ([]:_) = []
transpose m = map head m : transpose (map tail m)

intersect [] [] = []
intersect (x:xs) (y:ys) = if x == y then x : intersect xs ys else intersect xs ys

combine f g h xs ys = zipWith f (map g xs) (map h ys)
                                                                             
apply3 (f,g,s) (x,y,z) = (f x, g y, s z)

series _ _ 0 = []
series s f n = s : series (f s) f (n-1)

powerset [] = [[]]
powerset (x:xs) = (map (x:) s) ++ s where s = powerset xs

