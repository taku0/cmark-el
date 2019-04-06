[![License BSD 2 Clause][badge-license]][copying]
[![Build Status][badge-travis]][travis]

# cmark.el

CommonMark parser / HTML renderer.

This is a naive port of [commonmark.js](https://github.com/commonmark/commonmark.js/), the reference implementation.

## Installation

Download [latest release](https://github.com/taku0/cmark/releases) and execute `M-x package-install-file` for the .tar archive.

## Usage

```elisp
(require 'cmark)
(require 'cmark-html)

(let* ((parser (cmark-create-Parser))
       (root (cmark-parser-parse parser source-string))
       (walker (cmark-create-Node-walker root))
       (renderer (cmark-create-HtmlRenderer))
       event)
  (while (setq event (cmark--NodeWalker-next walker))
    (prin1 (cmark-event-entering event))
    (prin1 (cmark-Node-type (cmark-event-node event))))
  (princ (cmark-HtmlRenderer-render root)))
```

## Hacking

To build the package locally, run `make package`. You need [Cask](https://github.com/cask/cask).

To install the built package, run `make install`.

To run tests, run `make test`.

For other commands, run `make help`.

## License

BSD-2-Clause. Some files are licensed under other conditions.  See [LICENSE][] for details. Copyright (C) 2019 taku0.

[badge-license]: https://img.shields.io/badge/license-BSD--2--Clause-green.svg
[badge-travis]: https://travis-ci.org/taku0/cmark.png?branch=master
[travis]: https://travis-ci.org/taku0/cmark
[LICENSE]: ./LICENSE
