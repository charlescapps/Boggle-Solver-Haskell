--------------------------------------------------------------------------
Code for CS457/557 Functional Languages, Winter 2012, mpj@cs.pdx.edu

This file provides a type class for characterizing tree-like data types
and a small collection of generic operations, including some code that
can produce descriptions of tree values in dot format.

> module Treedot(
>    NodeId,          -- String
>    Tree(..),
>    LabeledTree(..),
>    depth,           -- :: Tree t => t -> Int
>    size,            -- :: Tree t => t -> Int
>    paths,           -- :: Tree t => t -> [[t]]
>    dfs,             -- :: Tree t => t -> [t]
>    toDot,           -- :: LabeledTree t => t -> String
>    listToDot,       -- :: LabeledTree t => NodeId -> [t] -> String
>  ) where

We characterize trees as types whose values can have zero or more
subtrees of the same type.

> class Tree t where
>   subtrees :: t -> [t]

Now we can define a whole bunch of interesting functions on trees in
a very generic way:

> depth  :: Tree t => t -> Int
> depth   = (1+) . foldl max 0 . map depth . subtrees

> size   :: Tree t => t -> Int
> size    = (1+) . sum . map size . subtrees

> paths               :: Tree t => t -> [[t]]
> paths t | null br    = [ [t] ]
>         | otherwise  = [ t:p | b <- br, p <- paths b ]
>           where br = subtrees t

> dfs    :: Tree t => t -> [t]
> dfs t   = t : concat (map dfs (subtrees t))

In each of these operations, the "Tree t => ..." portion of the type
indicates that the function works for any type t, so long as t is a
Tree type (i.e., so long as there is a definition of subtrees for t).

> class Tree t => LabeledTree t where
>   label :: t -> String

This gives us the machinery that we need to create a generic version
of the toDot function:

> data DotStmt = Node Path String
>              | Edge Path Path

> type Path    = [Int]
> type NodeId  = String

> showPath      :: Path -> String
> showPath p     = "\"" ++ show p ++ "\""

> graph      :: [DotStmt] -> String
> graph stmts = "digraph tree {\n" ++ semi (map toText stmts) ++ "}\n"
>  where semi = foldr (\l ls -> l ++ ";\n" ++ ls) ""
>        toText (Node p l)  = showPath p ++ " [label=\"" ++ l ++ "\"]"
>        toText (Edge p p') = showPath p ++ " -> " ++ showPath p'

> toDot         :: LabeledTree t => t -> String
> toDot          = graph . nodeTree []

> listToDot     :: LabeledTree t => NodeId -> [t] -> String
> listToDot n ts = graph (Node [] n
>                         : concat (zipWith (edgeTree []) [1..] ts))

> nodeTree      :: LabeledTree t => Path -> t -> [DotStmt]
> nodeTree p t   = Node p (label t)
>                : concat (zipWith (edgeTree p) [1..] (subtrees t))

> edgeTree      :: LabeledTree t => Path -> Int -> t -> [DotStmt]
> edgeTree p n c = Edge p p' : nodeTree p' c
>                  where p' = n : p

--------------------------------------------------------------------------
