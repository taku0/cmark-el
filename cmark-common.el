;;; cmark-common.el --- Common constants and functions. -*- lexical-binding: t -*-

;; Copyright (C) 2019 taku0, John MacFarlane

;; Author: taku0 (http://github.com/taku0)
;;         John MacFarlane
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

;; Common constants and functions used by cmark-blocks and cmark-inlines.

;;; Code


(require 'cmark-compat)

(require 'cmark-mdurl-encode)
(require 'cmark-mdurl-decode)

(defconst cmark--C_BACKSLASH ?\\)

(require 'cmark-entities)

(defconst cmark--ENTITY
  "&\\(?:#x[a-f0-9]\\{1,6\\}\\|#[0-9]\\{1,7\\}\\|[a-z][a-z0-9]\\{1,31\\}\\);")

(defconst cmark--TAGNAME "[A-Za-z][A-Za-z0-9-]*")
(defconst cmark--ATTRIBUTENAME "[a-zA-Z_:][a-zA-Z0-9:._-]*")
(defconst cmark--UNQUOTEDVALUE "[^\"'=<>`\x00-\x20]+")
(defconst cmark--SINGLEQUOTEDVALUE "'[^']*'")
(defconst cmark--DOUBLEQUOTEDVALUE "\"[^\"]*\"")
(defconst cmark--ATTRIBUTEVALUE
  (concat
   "\\(?:"
   cmark--UNQUOTEDVALUE
   "\\|"
   cmark--SINGLEQUOTEDVALUE
   "\\|"
   cmark--DOUBLEQUOTEDVALUE
   "\\)"))
(defconst cmark--ATTRIBUTEVALUESPEC
  (concat
   "\\(?:"
   cmark--SPACE
   "*="
   cmark--SPACE
   "*"
   cmark--ATTRIBUTEVALUE
   "\\)"))
(defconst cmark--ATTRIBUTE
  (concat
   "\\(?:"
   cmark--SPACE
   "+"
   cmark--ATTRIBUTENAME
   cmark--ATTRIBUTEVALUESPEC
   "?\\)"))
(defconst cmark--OPENTAG
  (concat
   "<"
   cmark--TAGNAME
   cmark--ATTRIBUTE
   "*"
   cmark--SPACE
   "*/?>"))
(defconst cmark--CLOSETAG
  (concat
   "</"
   cmark--TAGNAME
   cmark--SPACE
   "*[>]"))
(defconst cmark--HTMLCOMMENT
  "<!---->\\|<!--\\(?:-?[^>-]\\)\\(?:-?[^-]\\)*-->")
(defconst cmark--PROCESSINGINSTRUCTION "[<][?].*?[?][>]")
(defconst cmark--DECLARATION
  (concat
   "<![A-Z]+"
   cmark--SPACE
   "+[^>]*>"))
(defconst cmark--CDATA
  (concat
   "<!\\[CDATA\\[\\(?:"
   cmark--SPACE
   "\\|"
   cmark--NONSPACE
   "\\)*?\\]\\]>"))
(defconst cmark--HTMLTAG
  (concat
   "\\(?:"
   cmark--OPENTAG
   "\\|"
   cmark--CLOSETAG
   "\\|"
   cmark--HTMLCOMMENT
   "\\|"
   cmark--PROCESSINGINSTRUCTION
   "\\|"
   cmark--DECLARATION
   "\\|"
   cmark--CDATA
   "\\)"))
(defconst cmark--reHtmlTag (list (concat "\\`" cmark--HTMLTAG) :ignore-case))

(defconst cmark--reBackslashOrAmp "[\\&]")

(defconst cmark--ESCAPABLE "[]!\"#$%&'()*+,./:;<=>?@[\\^_`{|}~-]")

(defconst cmark--reEntityOrEscapedChar
  (list (concat
         "\\\\"
         cmark--ESCAPABLE
         "\\|"
         cmark--ENTITY)
        :ignore-case))

(defconst cmark--XMLSPECIAL "[&<>\"]")

(defconst cmark--reXmlSpecial cmark--XMLSPECIAL)

(defconst cmark--reXmlSpecialOrEntity
  (list (concat
         cmark--ENTITY
         "\\|"
         cmark--XMLSPECIAL)
        :ignore-case))

(defun cmark--unescapeChar (s)
  (if (eq (elt s 0) cmark--C_BACKSLASH)
      (cmark--charAt s 1)
    (cmark--decodeHTML s)))

(defun cmark--unescapeString (s)
  "Replace entities and backslash escapes with literal characters."
  (if (cmark--string-match cmark--reBackslashOrAmp s)
      (cmark--replaceAll cmark--reEntityOrEscapedChar #'cmark--unescapeChar s)
    s))

(defun cmark--normalizeURI (uri)
  (or (cmark--encode (cmark--decode uri)) uri))

(defun cmark--replaceUnsafeChar (s)
  (cond
   ((equal s "&") "&amp;")
   ((equal s "<") "&lt;")
   ((equal s ">") "&gt;")
   ((equal s "\"") "&quot;")
   (t s)))

(defun cmark--escapeXml (s preserve_entities)
  (if (cmark--string-match cmark--reXmlSpecial s)
      (if preserve_entities
          (cmark--replaceAll
           cmark--reXmlSpecialOrEntity
           #'cmark--replaceUnsafeChar
           s)
        (cmark--replaceAll
         cmark--reXmlSpecial
         #'cmark--replaceUnsafeChar
         s))
    s))

(provide 'cmark-common)

;;; cmark-common.el ends here
