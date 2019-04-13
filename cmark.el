;;; cmark.el --- CommonMark parser / HTML renderer. -*- lexical-binding: t -*-

;; Copyright (C) 2019 taku0

;; Author: taku0 (http://github.com/taku0)
;; Version: 0.28.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience
;; URL: https://github.com/taku0/cmark-el

;; This file is not part of GNU Emacs.

;; BSD 2-Clause License
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; A CommonMark parser / HTML renderer.
;;
;; This is a naive port of commonmark.js, the reference implementation.
;;
;; Usage:
;;
;; (let* ((root (cmark-parse source-string))
;;        (walker (cmark-create-Node-walker root))
;;        event)
;;   (while (setq event (cmark--NodeWalker-next walker))
;;     (prin1 (cmark-event-entering event))
;;     (prin1 (cmark-Node-type (cmark-event-node event))))
;;   (princ (cmark-render-html root)))

;;; Code:

(require 'cmark-blocks)
(require 'cmark-html)

(defun cmark-parse (source &rest options)
  "Parse CommonMark SOURCE then return the root Node.

OPTIONS are keyword arguments:
:smart SMART -- if SMART is non-nil, punctuations are handled smart.
:time TIME -- IF TIME is non-nil, dump time to parse."
  (cmark-Parser-parse
   (cmark-create-Parser (apply #'make-cmark-options options))
   source))

(defun cmark-render-html (document &rest options)
  "Convert DOCUMENT to a HTML string.

OPTIONS are keyword arguments:

:softbreak SOFTBREAK -- SOFTBREAK is a string to render softbreaks.
:safe SAFE -- if SAFE is non-nil, elimitate unsafe contents.
:sourcepos SOURCEPOS -- if SOURCEPOS is non-nil, embed source positions in
                        `data-sourcepos' attribute."
  (cmark-HtmlRenderer-render
   (cmark-create-HtmlRenderer (apply #'make-cmark-HtmlRenderer-options options))
   document))

(provide 'cmark)

;;; cmark.el ends here
