{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S
import Data.List

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst 

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd 

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.fromList (map head . group . sort $ (map snd (filter (\x -> if fst x == node then True else False) $ S.toList . edges $ graph)))

--plm graph = S.fromList $ map snd (filter (\x -> if fst x == 1 then True else False) $ S.toList . edges $ graph)
{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.fromList (map head . group . sort $ (map fst (filter (\x -> if snd x == node then True else False) $ S.toList . edges $ graph)))

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = 
    if (elem node $ S.toList $ nodes $ graph) 
    then fromComponents 
        (S.toList $ S.delete node $ nodes $ graph) 
        (filter (\x -> if fst x == node || snd x == node then False else True) $ S.toList $ edges $ graph) 
    else graph

--plm :: Ord a => a -> StandardGraph a -> Bool
--plm node graph = elem node $ S.toList $ nodes $ graph
--plm node graph = fromComponents $ S.toList $ S.delete node $ nodes $ graph $ S.toList $ (filter (\x -> if fst x == node || snd x == node then False else True) edges $ graph)
--plm node graph = filter (\x -> if fst x == node || snd x == node then False else True) $ S.toList $ edges $ graph

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = fromComponents (sort $ (S.toList $ S.delete old $ nodes $ graph) ++ news) 
    (sort (S.toList $ edges $ removeNode old graph) ++ 
    [(y, x) | x <- (S.toList $ outNeighbors old graph), y <- sort news] ++  
    [(x, y) | x <- (S.toList $ inNeighbors old graph), y <- sort news])

--plm old news graph = sort $ (S.toList $ S.delete old $ nodes $ graph) ++ news
--plm old news graph = [(x, y) | x <- (S.toList $ outNeighbors old graph), y <- sort news] 

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = fromComponents (addNode prop node graph) (finalList graph prop node)

addNode prop new graph = filter (\x -> negProp prop x) $ sort $ (S.toList $ nodes $ graph) ++ [new]

nodesToRemove prop graph = filter prop $ sort $ S.toList $ nodes $ graph
removeEdge elem listOfEdges = filter (\x -> if fst x == elem || snd x == elem then False else True) listOfEdges
removeEdges prop graph = foldr (\x acc -> removeEdge x acc) (S.toList $ edges $ graph) (nodesToRemove prop graph)    

edgeToSee graph prop = map head. group. sort $ 
    foldr (\x acc -> acc ++ (filter (\y -> if fst y == x || snd y == x then True else False) 
    (S.toList $ edges $ graph)) ) [] (nodesToRemove prop graph)

buildEdgeList graph prop new = let toRemove = nodesToRemove prop graph 
    in foldr (\x acc-> if (elem (fst x) toRemove) then (if (elem (snd x) toRemove) then acc ++ [(new, new)] else acc ++ [(new, snd x)]) else acc ++ [(fst x, new)]) 
    [] 
    (edgeToSee graph prop)

finalList graph prop new = map head . group . sort $ (removeEdges prop graph) ++ (buildEdgeList graph prop new)

negProp prop elem = if prop elem then False else True    

