;;; cmark-mdurl-encode.el --- URL encoder. -*- lexical-binding: t -*-

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

;; URL encoder.
;; This is almost `url-hexify-string' but skipping already encoded sequences.

;;; Code

(require 'url-util)

(defconst cmark--encode-allowed-chars
  (let ((vector (make-vector 256 nil)))
    (mapc (lambda (c) (aset vector c t)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;/?:@&=+$,-_.!~*'()#")
    vector))

(defun cmark--encode (string)
  "Encode unsafe characters with percent-encoding, skipping already
encoded sequences.

 - string       - string to encode"
  (cmark--replaceAll
   "%25\\([0-9a-fA-F][0-9a-fA-F]\\)"
   "%\\1"
   (url-hexify-string string cmark--encode-allowed-chars)))

(provide 'cmark-mdurl-encode)

;;; cmark-mdurl-encode.el ends here
