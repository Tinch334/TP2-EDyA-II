module ArrSeq where
import qualified Arr as A
import Par
import Seq


instance Seq A.Arr where

    emptyS = A.empty

    singletonS x = A.fromList [x]

    lengthS = A.length

    nthS s n = (s A.! n)

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
                | otherwise = 
                            let 
                                -- (?)
                                mid = div len 2 
                                (l', r') = takeS s mid ||| dropS s mid
                            in NODE l' r'
                where len = lengthS s

    showlS s    | lengthS s == 0 = NIL
                | otherwise = CONS (nthS s 0) (dropS s 1)

    joinS = A.flatten

    reduceS op b s = case (lengthS s) of
        0 -> b
        1 -> op b (nthS s 0)    
        _ -> reduceS op b (contractS op s)
            
    scanS op base seq = case (lengthS seq) of
        0 -> (emptyS, base)
        1 -> (singletonS base, op base (nthS seq 0))
        _ -> let 
                len = lengthS seq
                contracted = contractS op seq
                (cList, cRes) = scanS op base contracted
            in  (expandS op (lengthS seq) seq cList, cRes) 
    
    fromList = A.fromList

-- funciones auxiliares --
contractS op xs =
                let
                    n = lengthS xs
                    mid = div n 2
                    tam = div (n+1) 2
                    f j = if (j < mid ) then (op (nthS xs (2*j)) (nthS xs ((2 * j) + 1))) else (nthS xs (2*j))
                in A.tabulate f tam

expandS op n xs ys = A.tabulate f n
    where 
        f j = if even j then nthS ys (div j 2) else op (nthS ys (div j 2)) (nthS xs (j - 1))  
