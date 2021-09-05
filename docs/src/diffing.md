# Diffing

Difftastic treats diff calculations as a graph search problem. It
finds the minimal diff using Dijkstra's algorithm.

## Graph Representation

A vertex in the graph represents a position in two syntax trees.

The start vertex has both positions pointing to the first node in
their respective trees. The end node has both positions just after the
last node in the trees.

Consider comparing `A` with `X A`.

```
            Start:
            +---------------------+
            | Left: A  Right: X A |
            |      ^         ^    |
            +---------------------+
                   /       \
     Novel atom L /         \ Novel atom R
                 v           v
+---------------------+  +---------------------+
| Left: A  Right: X A |  | Left: A  Right: X A |
|        ^       ^    |  |      ^           ^  |
+---------------------+  +---------------------+
```

## Finding A Route

## Why Dijkstra?

Red Blob Games has a [great introduction to Dijkstra's algorithm for
path
finding](https://www.redblobgames.com/pathfinding/a-star/introduction.html#dijkstra). As
they note on [algorithm
changes](https://www.redblobgames.com/pathfinding/a-star/implementation.html#algorithm),
one big advantage of Dijkstra is that we don't need to construct the
graph in advance.

## Multiple Optimal Diffs

```
(A B
 C D)
```

```
(A B
 A B
 C D)
```

This is path dependent, so it requires an additional flag on graph nodes.

## Non-optimal Diffs

Function rewriting.

## Path-dependent Diffing

