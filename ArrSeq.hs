import Par
import Seq
import qualified Arr as A

instance Seq A.Arr where
    emptyS = A.empty

    singletonS x = A.fromList [x]

    lengthS s = A.length s

    nthS s n = s A.! n

    tabulateS f n = A.tabulate f n

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

    reduceS f b initialS = if lengthS initialS == 0 then b else f (reduceInner initialS) b where
        reduceInner s   | len == 1 = (nthS s 0)
                        | len == 2 = f (nthS s 0) (nthS s 1)
                        | otherwise = let
                            m = div len 2
                            (l, r) = (takeS s m, dropS s m)
                            (lr, rr) = reduceInner l ||| reduceInner r in
                                f lr rr
                        where len = lengthS s 

    scanS f b initialS = (contract initialS, b) where
        contract s  | len == 1 = s
                    | len == 2 = singletonS (f (nthS s 0) (nthS s 1))
                    | otherwise = let
                        m = div len 2
                        (l, r) = (takeS s m, dropS s m)
                        (lr, rr) = contract l ||| contract r in
                            appendS lr rr
                    where len = lengthS s

        contract2 s | lengthS s == 1 = s
                    | otherwise = let
                        (eval, r) = f (nthS s 0) (nthS s 1) ||| contract2 (dropS s 2)
                            appendS eval r

    fromList l = A.fromList l




lst :: A.Arr Int
lst = fromList [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6]
lst2 :: A.Arr Int
lst2 = fromList [-2, -1, 0, 1, 2]