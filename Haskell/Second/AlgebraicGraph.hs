module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node elem) = S.singleton elem
nodes (Overlay node1 node2) = S.union (nodes node1) (nodes node2)
nodes (Connect node1 node2) = S.union (nodes node1) (nodes node2)       

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node elem) = S.empty
edges (Overlay node1 node2) = S.union (edges node1) (edges node2)
edges (Connect node1 node2) = S.union (S.union (S.cartesianProduct (nodes node1) (nodes node2)) (edges node1)) (edges node2)

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node elem) = S.empty 
outNeighbors node (Overlay node1 node2) = S.union (outNeighbors node node1) (outNeighbors node node2)
outNeighbors node (Connect node1 node2) = if (S.member node (nodes node1)) then (S.union (nodes node2) (outNeighbors node node1)) else outNeighbors node node2         

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node elem) = S.empty
inNeighbors node (Overlay node1 node2) = S.union (inNeighbors node node1) (inNeighbors node node2)
inNeighbors node (Connect node1 node2) = if (S.member node (nodes node2)) then (S.union (nodes node1) (inNeighbors node node2)) else inNeighbors node node1
        

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node Empty = Empty
removeNode node (Node elem) = if elem == node then Empty else Node elem
removeNode node (Overlay node1 node2) = Overlay (removeNode node node1) (removeNode node node2) 
removeNode node (Connect node1 node2) = Connect (removeNode node node1) (removeNode node node2)         

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old news (Node elem) = if elem == old then overlay news else Node elem
splitNode old news (Overlay node1 node2) = Overlay (splitNode old news node1) (splitNode old news node2)
splitNode old news (Connect node1 node2) = Connect (splitNode old news node1) (splitNode old news node2)         

overlay [] = Empty
overlay [x] = Node x
overlay [x, y] = Overlay (Node x) (Node y)
overlay list = Overlay (Node (head list)) (overlay (tail list))  

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node Empty = Empty
mergeNodes prop node (Node elem) = if prop elem then Node node else Node elem
mergeNodes prop node (Overlay node1 node2) = Overlay (mergeNodes prop node node1) (mergeNodes prop node node2)
mergeNodes prop node (Connect node1 node2) = Connect (mergeNodes prop node node1) (mergeNodes prop node node2)        