# Displaying Diffs

Display techniques are just as important as the diff algorithm when a
user looks at results.

## Inline Diffs

This is the default display format for most diff tools. It scales well
to long lines and does not have alignment issues.

## Two-column Diffs

GNU diff includes a two-column (or 'side by side') display for changes.

```
$ diff -y --color=always sample_files/css_before.css sample_files/css_after.css
```

## Word Diffs

[diff-so-fancy](https://github.com/so-fancy/diff-so-fancy) adds
word-level highlighting to line-based diffs (plus additional fancy
formatting). It consumes the output of `git diff` directly rather than
calculating changes itself.

## Line Numbers

## Smart Hunk Headers

Git can use [regular expressions to extract the class/function
header](https://tekin.co.uk/2020/10/better-git-diff-output-for-ruby-python-elixir-and-more)
for the current hunk.

```diff
@@ -24,7 +24,7 @@ class TicketPdf
     ApplicationController.render(
       "tickets/index.html.haml",
       layout: "tickets",
-      assigns: { tickets: tickets }
+      assigns: { tickets: tickets, event_name: event_name }
     )
   end
```

Note how `class TicketPdf` is shown at the beginning.

## Syntax Highlighitng

[delta](https://github.com/dandavison/delta) performs line based
diffs, then uses levenshtein distance to highlight parts within the
line. It also offers syntax highlighting.

## HTML Display

E.g. GitHub, https://github.com/GumTreeDiff/gumtree and ydiff.
