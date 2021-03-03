quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort more
    where 
    less = filter (<=x) xs
    more = filter (>x) xs

movies = [("Batman",7.5,126),("Manhattan",8.0,96),("Alien",8.5,116),("Amadeus",8.3,160)]
getMovieName (n,_,_) = n
getMovieRating (_,r,_) = r
getMovieLength (_,_,l) = l

