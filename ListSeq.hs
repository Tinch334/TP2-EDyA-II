module ListSeq where
import Par
import Seq

-- sacar despues
tst f n = tstI 0 where
        tstI n' | n' >= n = []
                | otherwise = let (e, r) = f n' ||| tstI (n' + 1) in e:r

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
        if eval == True then x:r else r

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

    reduceS _ b [] = b
    reduceS f b (x:xs) = f x (reduceS f b xs)

    scanS f b s = let (lRes, fRes) = scanSInner (reverse s) in (reverse (tail lRes), fRes) where
        scanSInner [] = ([b], b)
        scanSInner (x:xs) = let
            (l, r) = scanSInner xs
            cRes = f x (nthS l 0) in (cRes:l, cRes)

    fromList xs = xs

    contract op (x:y:xs) = let ys, zs)

-- sacar despues
lst :: [Int]
lst = (appendS (singletonS 1) (singletonS 2))


test :: [Int]
test = [10..15]

fun :: Int -> (Int, Int)
fun j = (j, nthS test j)

test2 :: [(Int, Int)]
test2 = tabulateS fun (lengthS test)

test3 :: [Int]
test3 = takeS test 2


--lst2 :: [Int]
--lst2 = appendS lst (appendS (singletonS 3) (singletonS 4))

