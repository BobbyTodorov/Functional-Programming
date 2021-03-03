type Dish = (String,Int,Double)
getDishName (name,_,_) = name
getDishGr (_,gr,_) = gr
getDishPrice (_,_,price) = price

menu :: [Dish]
menu = [("bean soup", 300, 2.70), ("meatballs", 250, 3.00), ("yoghurt", 150, 1.50)]

--b)
getPricePerGlist :: [Dish] -> [(String, Double)]
getPricePerGlist [] = []
getPricePerGlist (mh:mt) = (getDishName mh, (getDishPrice mh) / fromIntegral (getDishGr mh)) : getPricePerGlist mt

maximum' []     = error "maximum of empty list?"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

removeElement [] elem = []
removeElement (h:t) elem
    |h == elem = t
    |otherwise = h : removeElement t elem


elemIndex [] elem = 100
elemIndex (h:t) elem
    |h == elem = 0
    |otherwise = 1 + elemIndex t elem


yoloH [] c m = []
yoloH l c m
    |maxDishPrice <= c = (fst max') : yoloH (removeElement l max') (c - maxDishPrice) m
    |otherwise = []
    where
        max' = maximum' l
        maxIndex = elemIndex l max'
        elemFromMenu = (m !! (maxIndex))
        maxDishPrice = (getDishPrice elemFromMenu)


yolo m c = yoloH (getPricePerGlist m) c m
        