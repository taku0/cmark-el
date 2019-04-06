;;; cmark-compat.el --- Utility functions. -*- lexical-binding: t -*-

;; Copyright (C) 2019 taku0

;; Author: taku0 (http://github.com/taku0)
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

;; Utility functions for cmark.

;;; Code

(defconst cmark--SPACE
  "[ \f\n\r\t\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]")

(defconst cmark--NONSPACE
  "[^ \f\n\r\t\v\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]")

(defun cmark--string-match (regexp string)
  (let ((regexp-string (if (consp regexp) (car regexp) regexp))
        (options (if (consp regexp) (cdr regexp) '())))
    (let ((case-fold-search (memq :ignore-case options)))
      (string-match regexp-string string))))

(defun cmark--replaceAll (regexp rep string)
  (let ((regexp-string (if (consp regexp) (car regexp) regexp))
        (options (if (consp regexp) (cdr regexp) '())))
    (let ((case-fold-search (memq :ignore-case options)))
      (replace-regexp-in-string regexp-string rep string t))))

(defun cmark--repeat (string count)
  (let ((result '()))
    (dotimes (_ count (apply #'concat result))
      (push string result))))

(defun cmark--charAt (string index)
  (if (< index (length string))
      (substring string index (1+ index))
    ""))

(defun cmark--trim (string)
  (cmark--replaceAll
   (concat
    "\\`"
    cmark--SPACE
    "+"
    "\\|"
    cmark--SPACE
    "+"
    "\\'")
   ""
   string))

(provide 'cmark-compat)

;;; cmark-compat.el ends here
