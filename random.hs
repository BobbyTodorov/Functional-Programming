import Data.List

var :: Int
var = 666

--znak ?
signF :: Int -> String
signF x 
    | x < 0 = "Negative"
    | x == 0 = "Zero"
    | otherwise = "Positive"

fact :: Int -> Int
fact n
    | n == 0 = 1
    | otherwise = n * fact (n - 1)

fact2 n = product [1..n]

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

diag :: (Int -> Int -> Int) -> Int -> Int
diag f x = f x x

fib :: Int -> Int
fib n = 
    if n < 0
        then error "asd"
    else if n == 0 || n == 1
        then 1
    else fib(n-1) + fib(n-2)

countRoots a b c 
    | d == 0    = "One Root"
    | d > 0     = "Two Roots"
    | otherwise = "No Roots"
    where d = (b * b - 4 * a * c)

--f s poredica ot neravenstva
fib2 1 = 1
fib2 2 = 1
fib2 n = fib2(n-1) + fib2(n-2)


fib3 n = loop 0 0 1
  where loop i curr next
          | i == n    = curr
          | otherwise = loop (i+1) next (curr+next)

power x i
    | i == 0 = 1
    | i < 0 = 1/power x (- i)
    | otherwise = x * power x (i - 1)

fastPower x i
    | i == 0 = 1
    | i == 1 = x
    | mod i 2 == 0 = halfI * halfI
    | otherwise = halfI * halfI * x
    where halfI = fastPower x (div i 2)

--test za korteji i obrazci
type Student = (String, Int, Int)
--st1 :: Student
--st2 :: Student
st1 = ("bobi", 45421, 6)
st2 = ("nqkoi", 12345, 2)

getFN (_,fn,_) = fn

betterGrade st1@(_,_,grade1) st2@(_,_,grade2)
    | grade1 > grade2 = st1
    | otherwise = st2


--vector sum only if positive coordinates
vector _ _ 0 = 0
vector _ 0 _ = 0
vector 0 _ _ = 0
vector a b c = a + b + c

vector' a b c d 
    | a == 0 || b == 0 || c == 0 || d == 0 = 0
    | otherwise = a + b + c + d

sumVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--lists
testl = [1,2,3,-3,-10,0,3,-3,-2]
testl2 = ['h', 'i']

mapAnti [] = []
mapAnti (h:ht) = h*(-1) : mapAnti ht

replicate' _ 0 = []
replicate' x i = x : replicate' x (i - 1)

take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

map' f lst = [f x | x<-lst ]
filter' p l = [ x | x<-l, p x]

prime 1 = False
prime n = null [ x | x<-[2..(n-1)], mod n x == 0]

primes l = filter' prime l

pairing lst1 lst2 = [ (x,y) | x<-lst1, y<-lst2 ]

primes' :: Integral a => [a]
primes' = sieve [2..]
  where sieve (x:xs) = x : (sieve $ filter (\y -> y`mod`x/=0) xs) -- ????

--flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

--pitagorovi troiki v interval [a;b] (uslovieto e dvusmisleno)
pitTupleGen a b = [ (x,y,z) | x<-[a..b], y<-[a..b], z<-[a..b], ((x^2 + y^2 == z^2) || (x^2 + z^2 == y^2) || (y^2 + z^2 == x^2))]

--some functs over lists
foldr' _ nv [] = nv
foldr' op nv (h:t) = h `op` (foldr' op nv t)

foldl'' _ nv [] = nv
foldl'' op nv (h:t) = foldl'' op (op nv h) t

twins = [ (x,x+2) | x <- [1..], prime x, prime (x+2) ]

--pitagorova troika, chrez bezkraen spisuk
pythagoreanTriples = [ (a,b,c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2 ]

--type Matrix a = [[a]]
--Matrix b = [[1],[2]]

--potok v intervala [a,b]
streamFromTo a b = [a..b]

--izpiti
{-
Задача 2. (Haskell/Scheme) Да се напише функция iterator l f, която проверява дали всеки елемент на списъка от числа l, без първия, се получава от предишния чрез едноместната числова функция f.
Пример:

iterator [ 3, 4, 5 ] (+1) → True

iterator [ 1, 2, 4 ] (+1) → False
-}

iterator [] f = error "empty list"
iterator [a] f = True
iterator (h:t) f =
    if (f(h) /= (head t))
        then False
    else 
        iterator t f

--2019 izpit
--3a
type Movie = (String,Double,Int)
movies = [("Batman", 7.5, 126), ("Manhattan", 7.5, 96), ("Alien", 8.5, 116), ("Amadeus", 8.3, 160)]

getMovieMin (_,_,min) = min
getMovieRating (_,rating,_) = rating
getMovieName (name,_,_) = name
shorterThanMin mins movies = [x | x<-movies, getMovieMin x <= mins]

lessThanMinMovieRatings mins movies = [getMovieRating x | x<-(shorterThanMin mins movies)]

bestMovie mins movies =
    let rating = maximum (lessThanMinMovieRatings mins movies)
    in
        [ getMovieName x | x<-movies, getMovieRating x == rating]

--3b
powerset [] = [[]]
powerset (x:xs) = xss ++ map (x:) xss
    where xss = powerset xs

sumOfSquares = [x^2+y^2|x<-[1..], y<-[1..]]

occurrances [] _ = []
occurrances _ [] = []
occurrances (h:t) l = [length (filter (==h) l)] ++ occurrances t l


mainDiag [] = []
mainDiag ((h:_):t) = h : mainDiag t 

--popravka 2019
--zad 1
similar l1 l2 = if (sort (nub l1)) == (sort (nub l2)) then True else False

existsSimilar x l1 = if (length [ y | y<-l1, similar x y]) > 1 then True else False
dissimilar l1 = [ x | x<-l1, (not (existsSimilar x l1))]

--zad 2
genByN n = [ x*n | x<-[1..]]
slepvane l = [((head l)*10 + (head (tail l)))] ++ (slepvane (tail (tail l)))
npairs n = tail (genByN n)

--zad 3
type Book = (String, String, Int)
dt = [("mat","prosveta",12),("bg","prosveta",11),("mat","prosveta",10),("geografiq","prosveta",10)]
st = [("bg","prosveta",12),("geografiq","asd",12),("istoriq","asd",12)]

getPredmet (predmet,_,_) = predmet
getKlas (_,_,klas) = klas
getIzdatelstvo (_,izd,_) = izd


exchange n dt st = ((dt \\ givenFromDarina) ++ givenFromStefan)
    where exchangeList = [ [x] ++ [y] | x<- dt, y<- st, getKlas y == n, (getPredmet x == getPredmet y) ]
          givenFromDarina = [ head $ x | x <- exchangeList ]
          givenFromStefan = [ head . tail $ x | x <- exchangeList ]

