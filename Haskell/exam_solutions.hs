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

foldr' _ [x] = x
foldr' func (x:xs) = func x (foldr' func xs)

foldl' _ [x] = x
foldl' func (x0:x1:xs) = foldl' func ((func x0 x1) :xs)

table func = [[func x y | y <- [0..]] | x<-[0..]]

subtable rows cols table = take rows (map (take cols) table)

fun _ _ _ 0 [] = 0
fun binary unary pred id (x:xs) = if pred x then binary (unary x) (fun binary unary pred id xs) else (fun binary unary pred id xs)

isPermutation [] _ = True
isPermutation (x:xs) ys = if check x ys then isPermutation xs ys else False

check _ [] = False
check elem (x:xs) = if elem == x then True else check elem xs

equiv n = ehelp 0 n where ehelp i k = if i < k then [x | x<-[i, n+i..]] : ehelp (i+1) k else []

--another way to solve equiv
equiv' n = [[n*j + k | j<-[0..]] | k<-[0..n-1]]

evalpoly [] _ = 0
evalpoly list var = evHelp list 0 var

evHelp [] _ _ = 0
evHelp (x:xs) expt var = (x * var^expt) + evHelp xs (expt + 1) var

--another way to write evalpoly
evalpoly [] _ = 0
evalpoly' (x:xs) var = x + var * evalpoly xs var


triples xs ys zs = [(x,y,z) | x<-xs, y<-ys, z<-zs]

pythagoras n = [(x, y, z) | x<-[1..n], y<-[x+1..n], z<-[y+1..n], x^2 + y^2 == z^2]

combine f g m = help f (map (help g) m) where help func (x:xs) = foldr func x xs

transpose ([]:_) = []
transpose m = map head m : transpose (map tail m)

mix f g m = g (map f (transpose m))

diffproduct [] = 1
diffproduct (x:xs) = product (map (x-) xs) * diffproduct xs

expand 0 = []
expand n = mod n 10 : expand (div n 10)

contract [] = 0
contract (x:xs) = x + 10 * contract xs   

appendAll [] = []
appendAll (x:xs) = x ++ appendAll xs

fasten [] _ = []
fasten (x:xs) (y:ys) = (x,y) : fasten xs ys

unfasten [] = []
unfasten ((x,y):zs) = (x:xs, y:ys) where (xs,ys) = unfasten zs

