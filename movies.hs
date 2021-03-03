movies = [("Batman",7.5,126),("Manhattan",8.0,96),("Alien",8.5,116),("Amadeus",8.3,160)]

getMovieName (n,_,_) = n
getMovieRating (_,r,_) = r
getMovieLength (_,_,l) = l

getMaxRatingMovie rating [] _ = rating
getMaxRatingMovie rating movies mins
    |newRating > rating && newLength <= mins = getMaxRatingMovie newRating (tail movies) mins
    |otherwise = getMaxRatingMovie rating (tail movies) mins
    where 
        newRating = getMovieRating (head movies)
        newLength = getMovieLength (head movies) 

getMovieByRating rating [] = ("",0.0,0)
getMovieByRating rating movies
    |rating == getMovieRating hm = hm
    |otherwise = getMovieByRating rating (tail movies)
    where hm = head movies

bestMovie mins movies = getMovieName (getMovieByRating (getMaxRatingMovie 0 movies mins) movies)
