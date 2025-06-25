{-
 Este módulo requiere la librería parallel. 
 
 Si no la tiene instalada, se puede instalarse utilizando Cabal, ejecutando el siguiente código  
 en un intérprete de comandos: 
 
 $ cabal update
 $ cabal install parallel
 
-}


module Par ((|||)) where

import Control.Parallel

infix 1 |||

(|||)   ::   a -> b -> (a,b)
a ||| b = a `par` b `par` (a,b)
-- sparkea a, retorna sin evaluar b `par` (a,b), entonces sparkea b y retorna (a,b) sin evaluar -- https://stackoverflow.com/questions/23954646/par-function-underlying-logic