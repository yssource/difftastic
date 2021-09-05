# Tree-Based Diffing

## Clojure: Autochrome

[Autochrome](https://fazzone.github.io/autochrome.html) parses Clojure
then performs a Dijkstra-based search to find the smallest diff. It
also includes a worked example showing the algorithm step-by-step.

## S-Expressions: Tree Diff

Tristan Hume [built a tree
diff](https://thume.ca/2017/06/17/tree-diffing/) during his internship
at Jane Street. It uses A* search and even applies patches.

This compares two s-expressions and builds a minimal new one with the
new section marked with `:date-switch`.

(This project is distinct from Jane Street's
[patdiff](https://github.com/janestreet/patdiff), which is a
line-oriented diff. It has some cleverness for distinguishing integers
and whitespace, but it doesn't understand that e.g. whitespace in
`"foo "` is meaningful).


### JSON: json-diff

[json-diff](https://github.com/andreyvit/json-diff) provides a
structural diff for JSON files. It terminates as soon as it sees a
difference, so e.g. `"foo"` and `["foo"]` are entirely different.

### graphtage

[graphtage](https://blog.trailofbits.com/2020/08/28/graphtage/)
compares structured data by parsing into a generic file format, then
displaying a diff. It finds the optimal edit sequence, and even allows
things like diffing JSON against YAML.


### Lisp diffs

[sdiff](https://fosdem.org/2021/schedule/event/sexpressiondiff/) and
[diff-sexp](https://web.archive.org/web/20160320134909/https://foldr.org/~michaelw/log/programming/lisp/diff-sexp)
explore s-expression oriented diffs.

### Python/Lisp: psydiff and ydiff

[psydiff](https://github.com/yinwang0/psydiff) and
[ydiff](https://github.com/yinwang0/ydiff) apply structural diffing to
Python and Lisp respectively, and output an HTML page of the
result. They consider ASTs excluding comments.


## Tree-sitter: Diffsitter

[Diffsitter](https://github.com/afnanenayet/diffsitter) is another
tree-sitter based diff tool. It uses [LCS diffing on the leaves of the
syntax
tree](https://github.com/afnanenayet/diffsitter/blob/b0fd72612c6fcfdb8c061d3afa3bea2b0b754f33/src/ast.rs#L310-L313).

## XML

There are several research papers exploring XML diff
techniques. ["Matching, diffing and merging
XML"](http://useless-factor.blogspot.com/2008/01/matching-diffing-and-merging-xml.html)
has a good overview of the papers available.

Some of these papers explore optimal (smallest) diffs, whereas others
intentionally produce non-optimal diffs to improve runtime performance.
