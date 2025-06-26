module ListSeq where
import Par
import Seq

instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS = (Prelude.length)

    nthS s n = s Prelude.!! n

    tabulateS f n = tabulateInner 0 where
        tabulateInner n'    | n' >= n = []
                            | otherwise = let (e, r) = f n' ||| tabulateInner (n' + 1) in e:r

    mapS _ [] = []
    mapS f (x:xs) = let (e, r) = f x ||| mapS f xs in e:r

    filterS _ [] = []
    filterS f (x:xs) = let (eval, r) = f x ||| filterS f xs in
        if eval then x:r else r

    appendS = (++)

    takeS = (flip take)

    dropS = (flip drop)
     
    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS s = 
        let 
            mid = div (lengthS s) 2 
            (l', r') = takeS s mid ||| dropS s mid
        in 
            NODE l' r'

    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS [] = []
    joinS (xs:xss) = appendS xs (joinS xss)

    
    reduceS f b [] = b
    reduceS f b [x] = f b x
    reduceS f b xs = reduceS f b (contractL f xs)

    scanS f b [] = (emptyS, b)
    scanS f b (x:[]) = (singletonS b, f b x)
    scanS op base seq = 
        let 
            len = lengthS seq
            contracted = contractL op seq
            (cList, cRes) = scanS op base contracted
        in  (expandL op seq cList, cRes)

    fromList xs = xs

-- funciones auxiliares --

toTreeS [] = EMPTY
toTreeS [x] = ELT x
toTreeS s = 
    let 
        mid = 2 ^ floor (logBase 2 (fromIntegral ((lengthS s) - 1)))
        (l', r') = takeS s mid ||| dropS s mid
    in 
        NODE l' r'

contractL f (x:y:zs) = 
    let 
        (xy, zs') = f x y ||| contractL f zs
    in  xy : zs'
contractL f zs = zs

expandL f xs (y:ys) = y : expandLodd xs (y:ys) where
    expandLodd (x:_:xs) (y:ys) = 
        let 
            (elem, rest) = (f y x) ||| expandLeven xs ys
        in elem : rest
    expandLodd _ _ = []
    expandLeven xs (y:ys) = y : expandLodd xs (y:ys)
    expandLeven _ _ = []