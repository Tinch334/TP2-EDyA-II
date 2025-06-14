import Par
import Seq

tst f n = tstI 0 where
        tstI n' | n' >= n = []
                | otherwise = let (e, r) = f n' ||| tstI (n' + 1) in e:r

instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS s = Prelude.length s

    nthS s n = s Prelude.!! n

    tabulateS f n = tabulateInner 0 where
        tabulateInner n'    | n' >= n = []
                            | otherwise = let (e, r) = f n' ||| tabulateInner (n' + 1) in e:r

    mapS _ [] = []
    mapS f (x:xs) = let (e, r) = f x ||| mapS f xs in e:r

    filterS _ [] = []
    filterS f (x:xs) = let (eval, r) = f x ||| filterS f xs in
        if eval == True then x:r else r

    appendS s1 s2 = s1 ++ s2

    takeS s n = take n s

    dropS s n = drop n s

    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS s = let mid = div (lengthS s) 2 in NODE (takeS s mid) (dropS s mid)

    showlS [] = NIL
    showlS (x:xs) = CONS x xs

    joinS [] = []
    joinS (xs:xss) = appendS xs (joinS xss)

    reduceS _ b [] = b
    reduceS f b (x:xs) = f x (reduceS f b xs)

    scanS f b s = let (lRes, fRes) = scanSInner (reverse s) in (reverse (tail lRes), fRes) where
        scanSInner [] = ([b], b)
        scanSInner (x:xs) = let
            (l, r) = scanSInner xs
            cRes = f x (nthS l 0) in (cRes:l, cRes)

    fromList xs = xs


lst :: [Int]
lst = (appendS (singletonS 1) (singletonS 2))
lst2 :: [Int]
lst2 = appendS lst (appendS (singletonS 3) (singletonS 4))