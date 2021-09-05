# Tricky Cases

Tree diffing is challenging in some cases. This is a list of problems
difftastic has encountered. Not all of these cases work well yet.

## Barfing, Slurping and Wrapping

```
x

;; Wrapping: highlight the parens.
(x)
```

```
(x)

;; Rewrapping: highlight the parens before and after
[x]
```

```
() x

;; Slurping: Highlight x before and after.
(x)
```

```
(x)

;; Barfing: Highlight x before and after.
() x
```

## Sliders

Sliders are a problem in line-based diffs. There are some common
heuristics (e.g. the "patience diff") to alleviate the common cases.

Tree-based diffs have a similar problem.

## Depth Changes

## Replacements

Not rewrites.

## Comments

Difftastic treats syntax precisely: the string `" "` is definitely not
the same as the string `"  "`.

The situation is less clear with comments.

```
// A single comment line.
```

```
// Additional content here.
// A second comment line.
```

In this case we want to match up `A single comment line` with `A
second comment line`, based on a fuzzy similarity. This allows
difftastic to use a word-based diff for the comment contents.

Multiline comments are awkward when they change indentation.

```
/* A multiline comment
 * whose indentation changes.
 */
```

```
if true {
    /* A multiline comment
     * whose indentation changes.
     */
}```

Block comments have prefixes that aren't meaningful.

```
/// The quick brown fox jumps over the lazy dog.
```

## Invalid Syntax

There's no guarantee that the input we're given is valid
syntax. Diffing uncommitted code is more likely to contain
mistakes. Even if the code is valid, it might use syntax that isn't
supported by the parser.

Tree-sitter uses error nodes, and difftastic does its best.

## Moves


