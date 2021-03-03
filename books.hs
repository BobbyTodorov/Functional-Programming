--zad 3
type Book = (String, String, Int)
dt = [("mat","prosveta",12),("bg","prosveta",11),("mat","prosveta",10),("geografiq","prosveta",12)]
st = [("bg","prosveta",12),("geografiq","asd",12),("mat","asd",12),("istoriq","wtF",10),("mat","asd2",12)]

getPredmet (predmet,_,_) = predmet
getKlas (_,_,klas) = klas
getIzdatelstvo (_,izd,_) = izd

removeElem [] _ = []
removeElem (l1h:l1t) elem
    |l1h == elem = removeElem l1t elem
    |otherwise = l1h : removeElem l1t elem

getContainedPredmet l1 elem
    |null l1 = ("","",0)
    |(getPredmet (head l1)) == (getPredmet elem) = (head l1)
    |otherwise = getContainedPredmet (tail l1) elem

containsPredmet l1 elem
    |null l1 = False
    |(getPredmet (head l1)) == (getPredmet elem) = True
    |otherwise = containsPredmet (tail l1) elem

exchange n dt st 
    |n < 1 || n > 12 = []
    |null dt || null st = dt
    |getKlas (head dt) == n = (head dt) : exchange n (tail dt) st
    |containsPredmet st (head dt) && (getKlas (getContainedPredmet st (head dt))) == n = exchange n ((getContainedPredmet st (head dt)):(tail dt)) st


uniqueing l1 
    |null l1 || length l1 == 1 = l1
    |otherwise = (head l1) : uniqueing (removeElem (tail l1) (head l1))

