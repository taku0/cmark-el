;;; cmark-mdurl-decode.el --- URL decoder. -*- lexical-binding: t -*-

;; Copyright (C) 2019 taku0, Vitaly Puzrin, Alex Kocharin

;; Author: taku0 (http://github.com/taku0)
;;         Vitaly Puzrin
;;         Alex Kocharin
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

;; URL decoder.
;; `url-unhex-string' does not handle unicodes well, so decode it manually.

;;; Code

(defun cmark--decode (string)
  (let ((start 0)
        (chunks '())
        match-beginning
        match-end)
    (while (string-match "%[0-9a-fA-F][0-9a-fA-F]" string start)
      (setq match-beginning (match-beginning 0))
      (setq match-end (match-end 0))
      (push (substring string start match-beginning) chunks)
      (push (byte-to-string
             (let ((code (string-to-number
                          (substring string
                                     (+ match-beginning 1)
                                     (+ match-beginning 3))
                          16)))
               (if (< code #x80)
                   code
                 ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Codes.html
                 (+ #x3FFF00 code))))
            chunks)
      (setq start match-end))
    (push (substring string start) chunks)
    (cmark--decode-utf-8 (mapconcat #'identity (nreverse chunks) ""))))

(defun cmark--decode-utf-8 (bytes)
  (let ((i 0)
        start
        (length (length bytes))
        b1
        b2
        b3
        b4
        code-point
        (chunks '()))
    (while (< i length)
      (setq start i)
      (while (and (< i length) (< (elt bytes i) #x3FFF80))
        (setq i (1+ i)))
      (push (substring bytes start i) chunks)

      (setq b1 (if (< i length) (logand (elt bytes i) #xFF) 0))
      (cond
       ((<= length i)
        t)

       ((= (logand b1 #xE0) #xC0)
        (setq b2 (if (< (+ i 1) length) (logand (elt bytes (+ i 1)) #xFF) 0))
        (setq code-point
              (logior (logand (lsh b1 6) #x7C0)
                      (logand b2 #x3F)))
        (if (and (= (logand b2 #xC0) #x80)
                 (<= #x80 code-point))
            (progn
              (push (string code-point) chunks)
              (setq i (+ i 1)))
          (push (string #xFFFD) chunks)))

       ((= (logand b1 #xF0) #xE0)
        (setq b2 (if (< (+ i 1) length) (logand (elt bytes (+ i 1)) #xFF) 0))
        (setq b3 (if (< (+ i 2) length) (logand (elt bytes (+ i 2)) #xFF) 0))
        (setq code-point
              (logior (logand (lsh b1 12) #xF000)
                      (logand (lsh b2 6) #xFC0)
                      (logand b3 #x3F)))
        (if (and (= (logand b2 #xC0) #x80)
                 (= (logand b3 #xC0) #x80)
                 (<= #x800 code-point))
            (progn
              (push (string code-point) chunks)
              (setq i (+ i 2)))
          (push (string #xFFFD) chunks)))

       ((= (logand b1 #xF8) #xF0)
        (setq b2 (if (< (+ i 1) length) (logand (elt bytes (+ i 1)) #xFF) 0))
        (setq b3 (if (< (+ i 2) length) (logand (elt bytes (+ i 2)) #xFF) 0))
        (setq b4 (if (< (+ i 3) length) (logand (elt bytes (+ i 3)) #xFF) 0))
        (setq code-point
              (logior (logand (lsh b1 18) #x1C0000)
                      (logand (lsh b2 12) #x3F000)
                      (logand (lsh b3 6) #xFC0)
                      (logand b4 #x3F)))
        (if (and (= (logand b2 #xC0) #x80)
                 (= (logand b3 #xC0) #x80)
                 (<= #x10000 code-point))
            (progn
              (push (string code-point) chunks)
              (setq i (+ i 3)))
          (push (string #xFFFD) chunks)))
       (t
        (push (string #xFFFD) chunks)))
      (setq i (1+ i)))
    (mapconcat #'identity (nreverse chunks) "")))

(provide 'cmark-mdurl-decode)

;;; cmark-mdurl-decode.el ends here
