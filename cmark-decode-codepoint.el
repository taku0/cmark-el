;;; cmark-decode-codepoint.el --- CommonMark parser. -*- lexical-binding: t -*-

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

;; Decoder for HTML5 numeric character references.

;;; Code

(defconst cmark--decodeMap
  (let ((map (make-hash-table)))
    ;; https://www.w3.org/TR/html52/syntax.html#tokenizer-numeric-character-reference-end-state
    (puthash #x00 #xfffd map) ;; REPLACEMENT CHARACTER
    (puthash #x80 #x20ac map) ;; EURO SIGN
    (puthash #x82 #x201a map) ;; SINGLE LOW-9 QUOTATION MARK
    (puthash #x83 #x0192 map) ;; LATIN SMALL LETTER F WITH HOOK
    (puthash #x84 #x201e map) ;; DOUBLE LOW-9 QUOTATION MARK
    (puthash #x85 #x2026 map) ;; HORIZONTAL ELLIPSIS
    (puthash #x86 #x2020 map) ;; DAGGER
    (puthash #x87 #x2021 map) ;; DOUBLE DAGGER
    (puthash #x88 #x02c6 map) ;; MODIFIER LETTER CIRCUMFLEX ACCENT
    (puthash #x89 #x2030 map) ;; PER MILLE SIGN
    (puthash #x8a #x0160 map) ;; LATIN CAPITAL LETTER S WITH CARON
    (puthash #x8b #x2039 map) ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (puthash #x8c #x0152 map) ;; LATIN CAPITAL LIGATURE OE
    (puthash #x8e #x017d map) ;; LATIN CAPITAL LETTER Z WITH CARON
    (puthash #x91 #x2018 map) ;; LEFT SINGLE QUOTATION MARK
    (puthash #x92 #x2019 map) ;; RIGHT SINGLE QUOTATION MARK
    (puthash #x93 #x201c map) ;; LEFT DOUBLE QUOTATION MARK
    (puthash #x94 #x201d map) ;; RIGHT DOUBLE QUOTATION MARK
    (puthash #x95 #x2022 map) ;; BULLET
    (puthash #x96 #x2013 map) ;; EN DASH
    (puthash #x97 #x2014 map) ;; EM DASH
    (puthash #x98 #x02dc map) ;; SMALL TILDE
    (puthash #x99 #x2122 map) ;; TRADE MARK SIGN
    (puthash #x9a #x0161 map) ;; LATIN SMALL LETTER S WITH CARON
    (puthash #x9b #x203a map) ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (puthash #x9c #x0153 map) ;; LATIN SMALL LIGATURE OE
    (puthash #x9e #x017e map) ;; LATIN SMALL LETTER Z WITH CARON
    (puthash #x9f #x0178 map) ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
    map))

;; modified version of https://github.com/mathiasbynens/he/blob/master/src/he.js#L94-L119
(defun cmark--decodeCodePoint (codePoint)
  (if (or (and (>= codePoint 55296) (<= codePoint 57343))
          (> codePoint 1114111))
      "\uFFFD"
    (when (gethash codePoint cmark--decodeMap)
      (setq codePoint (gethash codePoint cmark--decodeMap)))
    (string codePoint)))

(provide 'cmark-decode-codepoint)

;;; cmark-decode-codepoint.el ends here
