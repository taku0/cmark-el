;;; cmark-entities.el --- Decoder for HTML entities. -*- lexical-binding: t -*-

;; Copyright (C) 2019 taku0, Felix Böhm

;; Author: taku0 (http://github.com/taku0)
;;         Felix Böhm
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

;; Decoder for HTML entities.

;;; Code


(require 'json)
(require 'cmark-decode-codepoint)

(defconst cmark--maps-directory
  (concat (file-name-as-directory
           (file-name-directory
            (or load-file-name buffer-file-name)))
          "maps"))

(defconst cmark--entityMap
  (let ((json-object-type 'hash-table)
        (json-key-type 'string))
    (json-read-file (concat (file-name-as-directory cmark--maps-directory)
                            "entities.json"))))

(defconst cmark--legacyMap
  (let ((json-object-type 'hash-table)
        (json-key-type 'string))
    (json-read-file (concat (file-name-as-directory cmark--maps-directory)
                            "legacy.json"))))

;; The regexp in entities.js is too big for Emacs, so handle it manually.
(defconst cmark--decodeHTML-re (concat
                                "&"
                                "\\(?:"
                                "[a-zA-Z][0-9a-zA-Z]*;?"
                                "\\|"
                                "#[xX][0-9a-fA-F]+;?"
                                "\\|"
                                "#[0-9]+;?"
                                "\\)"))

(defun cmark--decodeHTML-replacer (str)
  (let* ((end-with-semicolon (eq (elt str (1- (length str))) ?\;))
         (name (substring str 1 (- (length str) (if end-with-semicolon 1 0)))))
    (cond ((eq (elt name 0) ?#)
           (cmark--decodeCodePoint
            (if (or (eq (elt name 1) ?X) (eq (elt name 1) ?x))
                (string-to-number (substring name 2) 16)
              (string-to-number (substring name 1) 10))))
          ((or end-with-semicolon
               (gethash name cmark--legacyMap))
           (or (gethash name cmark--entityMap) str))
          (t
           str))))

(defun cmark--decodeHTML (s)
  (cmark--replaceAll cmark--decodeHTML-re #'cmark--decodeHTML-replacer s))

(provide 'cmark-entities)

;;; cmark-entities.el ends here
