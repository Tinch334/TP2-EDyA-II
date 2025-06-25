import Par
import Seq
import qualified Arr as A

instance Seq A.Arr where
    emptyS = A.empty

    singletonS x = A.fromList [x]

    lengthS s = A.length s

    nthS s n = s A.! n

    tabulateS = A.tabulate

    mapS f s = A.tabulate (\x -> f (nthS s x)) (lengthS s)

    filterS f s | len == 0 = emptyS
                | len == 1 = if f (nthS s 0) then s else emptyS
                | otherwise = let
                    m = div len 2
                    (l, r) = (takeS s m, dropS s m)
                    (lr, rr) = filterS f l ||| filterS f r in
                        appendS lr rr
                where len = lengthS s

    appendS s1 s2 = joinS (A.fromList [s1, s2])

    takeS s n = A.subArray 0 n s

    dropS s n = A.subArray n ((lengthS s) - n) s

    showtS s    | len == 0 = EMPTY
                | len == 1 = ELT (nthS s 0)
                | otherwise = let mid = div len 2 in NODE (takeS s mid) (dropS s mid)
                where len = lengthS s

    showlS s    | lengthS s == 0 = NIL
                | otherwise = CONS (nthS s 0) (dropS s 1)

    joinS s = A.flatten s

    -- da error de tipoo
    reduceS op e s = case (lengthS s) of
        0 -> e
        1 -> op e (nthS s 0)
        _ -> reduceS op e (contractS s)
        where
            contractS xs = 
                let
                    n = lengthS xs
                    mid = ceiling ((fromIntegral n) / 2)
                    tamCont = div (n+1) 2 -- longitud de la version contraida (ceil (n/2))
                    fun = (\j -> if (j < mid) then op (nthS xs (2*j)) (nthS xs ((2 * j) + 1))  else nthS xs (2*j)) :: Int -> a
                in
                    (tabulateS fun tamCont) :: (A.Arr a)


    --scanS f b initialS = (contract initialS, b) where
    scanS = undefined


    fromList l = A.fromList l


lst :: A.Arr Int
lst = fromList [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6]
lst2 :: A.Arr Int
lst2 = fromList [-2, -1, 0, 1, 2]
lst3 :: A.Arr Int
lst3 = fromList [1..10]