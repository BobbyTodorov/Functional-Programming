--a)
contains l elem
    |null l = False
    |elem == head l = True
    |otherwise = contains (tail l) elem

similarTo [] [] = True
similarTo [] _ = False
similarTo _ [] = False
similarTo l1 l2
    |length l1 == 1 && contains l2 (head l1) = True
    |contains l2 (head l1) = similarTo (tail l1) l2
    |otherwise = False


similar l1 l2 = similarTo l1 l2 && similarTo l2 l1

--b)
deleteFromL [] _ = []
deleteFromL (l1h:l1t) elem
    |l1h /= elem = l1h : deleteFromL l1t elem
    |otherwise = l1t

similarToL [] [[]] = True
similarToL l1 l2
    |length l2 == 1 && (not (similar l1 (head l2))) = False
    |length l2 == 1 && similar l1 (head l2) = True
    |similar l1 (head l2) = True
    |otherwise = similarToL l1 (tail l2)

dissimilar [] = []
dissimilar [[]] = []
dissimilar l1 = [ x | x <- l1, (not (similarToL x (deleteFromL l1 x)))]