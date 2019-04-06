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
;; (let* ((parser (cmark-create-Parser))
;;        (root (cmark-parser-parse parser source-string))
;;        (walker (cmark-create-Node-walker root))
;;        (renderer (cmark-create-HtmlRenderer))
;;        event)
;;   (while (setq event (cmark--NodeWalker-next walker))
;;     (prin1 (cmark-event-entering event))
;;     (prin1 (cmark-Node-type (cmark-event-node event))))
;;   (princ (cmark-HtmlRenderer-render root)))
;;
;; To generate HTML, require cmark-html.

;;; Code:

(require 'cmark-blocks)

(provide 'cmark)

;;; cmark.el ends here
