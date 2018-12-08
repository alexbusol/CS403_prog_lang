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
