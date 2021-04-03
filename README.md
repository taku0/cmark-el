[![License BSD 2 Clause][badge-license]][LICENSE]
[![Build Status][badge-travis]][travis]

# cmark.el

CommonMark parser / HTML renderer.

This is a naive port of [commonmark.js][], the reference implementation of
CommonMark in JavaScript.

## Installation

Download [latest release][releases] and execute `M-x package-install-file` for
the .tar archive.

## Usage

Instead of converting Markdown directly to HTML, as most converters
do, `commonmark.js` parses Markdown to an AST (abstract syntax tree),
and then renders this AST as HTML.  This opens up the possibility of
manipulating the AST between parsing and rendering.  For example, one
could transform emphasis into ALL CAPS.

Here's a basic usage example:

``` elisp
(require 'cmark)

(let ((parsed (cmark-parse "Hello *world*")) ;; parsed is a 'Node' tree
      result)
  ;; transform parsed if you like...

  (setq result (cmark-render-html parsed))) ;; result is a String
```

`cmark-parse` and `cmark-render-html` take optional parameters:

``` elisp
(cmark-parse "abc" :smart t)
(cmark-render-html parsed :sourcepos t)
```

`cmark-parse` currently supports the following:

- `smart`:  if non-`nil`, straight quotes will be made curly, `--` will
  be changed to an en dash, `---` will be changed to an em dash, and
  `...` will be changed to ellipses.

`cmark-render-html` supports these options:

- `sourcepos`:  if non-`nil`, source position information for block-level
  elements will be rendered in the `data-sourcepos` attribute.
- `safe`: if non-`nil`, raw HTML will not be passed through to HTML
  output (it will be replaced by comments), and potentially unsafe
  URLs in links and images (those beginning with `javascript:`,
  `vbscript:`, `file:`, and with a few exceptions `data:`) will
  be replaced with empty strings.
- `softbreak`: specify raw string to be used for a softbreak.
- `esc`: specify a function to be used to escape strings.  Its
  argument is the string.


For example, to make soft breaks render as hard breaks in HTML:

``` elisp
(cmark-render-html parsed :softbreak "<br />")
```

To make them render as spaces:

``` js
(cmark-render-html parsed :softbreak " ")
```

The parser returns a `cmark-Node`.  The following public properties are defined
(those marked "read-only" have only a getter, not a setter):

- `type` (read-only):  a String, one of
  `text`, `softbreak`, `linebreak`, `emph`, `strong`,
  `html_inline`, `link`, `image`, `code`, `document`, `paragraph`,
  `block_quote`, `item`, `list`, `heading`, `code_block`,
  `html_block`, `thematic_break`.
- `firstChild` (read-only):  a Node or `nil`.
- `lastChild` (read-only): a Node or `nil`.
- `next` (read-only): a Node or `nil`.
- `prev` (read-only): a Node or `nil`.
- `parent` (read-only): a Node or `nil`.
- `sourcepos` (read-only): an cons cell with the following form:
  `((startline . startcolumn) . (endline . endcolumn))`.
- `isContainer` (read-only): non-`nil` if the Node can contain other
   Nodes as children.
- `literal`: the literal String content of the node or `nil`.
- `destination`: link or image destination (String) or `nil`.
- `title`: link or image title (String) or `nil`.
- `info`: fenced code block info string (String) or `nil`.
- `level`: heading level (Number).
- `listType`: a String, either `bullet` or `ordered`.
- `listTight`: non-`nil` if list is tight.
- `listStart`: a Number, the starting number of an ordered list.
- `listDelimiter`: a String, either `)` or `.` for an ordered list.
- `onEnter`, `onExit`: Strings, used only for `custom_block` or
  `custom_inline`.

Nodes have the following public methods:

- `(cmark-Node-appendChild node child)`:  Append a Node `child` to the end of
  the Node's children.
- `(cmark-Node-prependChild node child)`:  Prepend a Node `child` to the
  beginning of the Node's children.
- `(cmark-Node-unlink node)`:  Remove the Node from the tree, severing its links
  with siblings and parents, and closing up gaps as needed.
- `(cmark-Node-insertAfter node sibling)`: Insert a Node `sibling` after the
  Node.
- `(cmark-Node-insertBefore node sibling)`: Insert a Node `sibling` before the
  Node.
- `(cmark-Node-walker node)`: Returns a NodeWalker that can be used to iterate
  through the Node tree rooted in the Node.

The `cmark-NodeWalker` returned by `cmark-Node-walker` has two methods:

- `(cmark-NodeWalker-next walker)`: Returns an `cmark-event` with properties
  `entering` (a boolean, which is non-`nil` when we enter a Node from a parent
  or sibling, and `nil` when we reenter it from a child).  Returns `nil` when
  we have finished walking the tree.
- `(cmark-NodeWalker-resumeAt walker node entering)`: Resets the iterator to
  resume at the specified node and setting for `entering`.  (Normally this isn't
  needed unless you do destructive updates to the Node tree.)

Here is an example of the use of a NodeWalker to iterate through
the tree, making transformations.  This simple example converts
the contents of all `text` nodes to ALL CAPS:

``` elisp
(let ((walker (cmark-Node-walker parsed))
      event
      node)
  (while (setq event (cmark-NodeWalker-next walker))
    (setq node (cmark-event-node event))
    (when (and (cmark-event-entering event)
               (equal (cmark-Node-type node) "text"))
      (setf (cmark-Node-literal node) (upcase (cmark-Node-literal node))))))
```

This more complex example converts emphasis to ALL CAPS:

``` elisp
(let ((walker (cmark-Node-walker parsed))
      event
      node
      (inEmph nil))
  (while (setq event (cmark-NodeWalker-next walker))
    (setq node (cmark-event-node event))
    (cond
     ((equal (cmark-Node-type node) "emph")
      (if (cmark-event-entering event)
          (setq inEmph t)
        (setq inEmph nil)
        ;; add Emph node's children as siblings
        (while (cmark-Node-firstChild node)
          (cmark-Node-insertBefore node (cmark-Node-firstChild node)))
        ;; remove the empty Emph node
        (cmark-Node-unlink node)))
     ((and inEmph (equal (cmark-Node-type node) "text"))
      (setf (cmark-Node-literal node)
            (upcase (cmark-Node-literal node)))))))
```

Exercises for the reader:  write a transform to

1. De-linkify a document, transforming links to regular text.
2. Remove all raw HTML (`html_inline` and `html_block` nodes).
3. Run fenced code blocks marked with a language name through
   a syntax highlighting library, replacing them with an `HtmlBlock`
   containing the highlighted code.
4. Print warnings to the console for images without image
   descriptions or titles.

## A note on security

The library does not attempt to sanitize link attributes or
raw HTML.  If you use this library in applications that accept
untrusted user input, you should either enable the `safe` option
(see above) or run the output through an HTML sanitizer to protect against
[XSS attacks](http://en.wikipedia.org/wiki/Cross-site_scripting).

## Hacking

To build the package locally, run `make package`. You need [Cask][].

To install the built package, run `make install`.

To run tests, run `make test`.

For other commands, run `make help`.

## License

BSD-2-Clause. Some files are licensed under other conditions.
See [LICENSE][] for details. Copyright (C) 2019 taku0 and original
`commonmark.js` authors.

[badge-license]: https://img.shields.io/badge/license-BSD--2--Clause-green.svg
[badge-travis]: https://travis-ci.org/taku0/cmark-el.png?branch=master
[travis]: https://travis-ci.org/taku0/cmark-el
[LICENSE]: ./LICENSE
[commonmark.js]: https://github.com/commonmark/commonmark.js/
[releases]: https://github.com/taku0/cmark/releases
[Cask]: https://github.com/cask/cask
