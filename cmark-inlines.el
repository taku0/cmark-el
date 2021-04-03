;;; cmark-inlines.el --- Parser for inline markups. -*- lexical-binding: t -*-

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

;; Parser for inline markups.

;;; Code

(require 'cmark-compat)

(require 'cmark-node)
(require 'cmark-common)
(require 'cmark-from-code-point)
(require 'cmark-entities)

;;; Constants for character codes:

(defconst cmark--C_NEWLINE 10)
(defconst cmark--C_ASTERISK 42)
(defconst cmark--C_UNDERSCORE 95)
(defconst cmark--C_BACKTICK 96)
(defconst cmark--C_OPEN_BRACKET 91)
(defconst cmark--C_CLOSE_BRACKET 93)
(defconst cmark--C_LESSTHAN 60)
(defconst cmark--C_BANG 33)
(defconst cmark--C_BACKSLASH 92)
(defconst cmark--C_AMPERSAND 38)
(defconst cmark--C_OPEN_PAREN 40)
(defconst cmark--C_CLOSE_PAREN 41)
(defconst cmark--C_COLON 58)
(defconst cmark--C_SINGLEQUOTE 39)
(defconst cmark--C_DOUBLEQUOTE 34)

;;; Some regexps used in inline parser:

(defconst cmark--ESCAPED_CHAR (concat "\\\\" cmark--ESCAPABLE))

(defconst cmark--rePunctuation
  (concat "\\`[]!\"#$%&'()*+,./:;<=>?@[\\^_`{|}~\xA1\xA7\xAB\xB6\xB7\xBB\xBF\u037E\u0387\u055A-\u055F\u0589\u058A\u05BE\u05C0\u05C3\u05C6\u05F3\u05F4\u0609\u060A\u060C\u060D\u061B\u061E\u061F\u066A-\u066D\u06D4\u0700-\u070D\u07F7-\u07F9\u0830-\u083E\u085E\u0964\u0965\u0970\u0AF0\u0DF4\u0E4F\u0E5A\u0E5B\u0F04-\u0F12\u0F14\u0F3A-\u0F3D\u0F85\u0FD0-\u0FD4\u0FD9\u0FDA\u104A-\u104F\u10FB\u1360-\u1368\u1400\u166D\u166E\u169B\u169C\u16EB-\u16ED\u1735\u1736\u17D4-\u17D6\u17D8-\u17DA\u1800-\u180A\u1944\u1945\u1A1E\u1A1F\u1AA0-\u1AA6\u1AA8-\u1AAD\u1B5A-\u1B60\u1BFC-\u1BFF\u1C3B-\u1C3F\u1C7E\u1C7F\u1CC0-\u1CC7\u1CD3\u2010-\u2027\u2030-\u2043\u2045-\u2051\u2053-\u205E\u207D\u207E\u208D\u208E\u2308-\u230B\u2329\u232A\u2768-\u2775\u27C5\u27C6\u27E6-\u27EF\u2983-\u2998\u29D8-\u29DB\u29FC\u29FD\u2CF9-\u2CFC\u2CFE\u2CFF\u2D70\u2E00-\u2E2E\u2E30-\u2E42\u3001-\u3003\u3008-\u3011\u3014-\u301F\u3030\u303D\u30A0\u30FB\uA4FE\uA4FF\uA60D-\uA60F\uA673\uA67E\uA6F2-\uA6F7\uA874-\uA877\uA8CE\uA8CF\uA8F8-\uA8FA\uA8FC\uA92E\uA92F\uA95F\uA9C1-\uA9CD\uA9DE\uA9DF\uAA5C-\uAA5F\uAADE\uAADF\uAAF0\uAAF1\uABEB\uFD3E\uFD3F\uFE10-\uFE19\uFE30-\uFE52\uFE54-\uFE61\uFE63\uFE68\uFE6A\uFE6B\uFF01-\uFF03\uFF05-\uFF0A\uFF0C-\uFF0F\uFF1A\uFF1B\uFF1F\uFF20\uFF3B-\uFF3D\uFF3F\uFF5B\uFF5D\uFF5F-\uFF65-]"
          "\\|"
          "\uD800[\uDD00-\uDD02\uDF9F\uDFD0]"
          "\\|"
          "\uD801\uDD6F"
          "\\|"
          "\uD802[\uDC57\uDD1F\uDD3F\uDE50-\uDE58\uDE7F\uDEF0-\uDEF6\uDF39-\uDF3F\uDF99-\uDF9C]"
          "\\|"
          "\uD804[\uDC47-\uDC4D\uDCBB\uDCBC\uDCBE-\uDCC1\uDD40-\uDD43\uDD74\uDD75\uDDC5-\uDDC9\uDDCD\uDDDB\uDDDD-\uDDDF\uDE38-\uDE3D\uDEA9]"
          "\\|"
          "\uD805[\uDCC6\uDDC1-\uDDD7\uDE41-\uDE43\uDF3C-\uDF3E]"
          "\\|"
          "\uD809[\uDC70-\uDC74]"
          "\\|"
          "\uD81A[\uDE6E\uDE6F\uDEF5\uDF37-\uDF3B\uDF44]"
          "\\|"
          "\uD82F\uDC9F"
          "\\|"
          "\uD836[\uDE87-\uDE8B]"))

(defconst cmark--reLinkTitle
  (concat
   "\\`"
   "\\(?:"
   "\"\\(" cmark--ESCAPED_CHAR "\\|[^\"\x00]\\)*\""
   "\\|"
   "'\\(" cmark--ESCAPED_CHAR "\\|[^'\x00]\\)*'"
   "\\|"
   "(\\(" cmark--ESCAPED_CHAR "\\|[^()\x00]\\)*)"
   "\\)"))

(defconst cmark--reLinkDestinationBraces
  "\\`\\(?:<\\(?:[^<>\n\\\x00]\\|\\.\\)*>\\)")

(defconst cmark--reEscapable (concat "\\`" cmark--ESCAPABLE))

(defconst cmark--reEntityHere (list (concat "\\`" cmark--ENTITY) :ignore-case))

(defconst cmark--reTicks "`+")

(defconst cmark--reTicksHere "\\``+")

(defconst cmark--reEllipses "\\.\\.\\.")

(defconst cmark--reDash "--+")

(defconst cmark--reEmailAutolink
  (concat "\\`"
         "<"
         "\\("
         "[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+"
         "@"
         "[a-zA-Z0-9]\\(?:[a-zA-Z0-9-]\\{0,61\\}[a-zA-Z0-9]\\)?\\(?:\\.[a-zA-Z0-9]\\(?:[a-zA-Z0-9-]\\{0,61\\}[a-zA-Z0-9]\\)?\\)*"
         "\\)"
         ">"))

(defconst cmark--reAutolink
  (list "\\`<[A-Za-z][A-Za-z0-9.+-]\\{1,31\\}:[^<>\x00-\x20]*>" :ignore-case))

(defconst cmark--reSpnl "\\` *\\(?:\n *\\)?")

(defconst cmark--reWhitespaceChar "\\`[ \t\n\x0b\x0c\x0d]")

(defconst cmark--reUnicodeWhitespaceChar (concat "\\`" cmark--SPACE))

(defconst cmark--reFinalSpace " *$")

(defconst cmark--reInitialSpace "\\` *")

(defconst cmark--reSpaceAtEndOfLine "\\` *\\(\\'\\|\n\\)")

;; [^z-a] is "any char including newline".
(defconst cmark--reLinkLabel "\\`\\[\\(?:[^][\\]\\|\\\\[^z-a]\\)\\{0,1000\\}]")

;; Matches a string of non-special characters.
(defconst cmark--reMain "^[^][\n`\\\\!<&*_'\"]+")

(defun cmark--text (s)
  (let ((node (cmark-create-Node "text")))
    (setf (cmark-Node-_literal node) s)
    node))


;; normalize a reference in reference link (remove []s, trim,
;; collapse internal space, unicode case fold.
;; See commonmark/commonmark.js#168.
(defun cmark--normalizeReference (string)
  (upcase
   (downcase
    (cmark--replaceAll
     "[ \t\r\n]+"
     " "
     (cmark--trim (substring string 1 (1- (length string))))))))

;;; INLINE PARSER

(cl-defstruct cmark--delimiters
  cc
  numdelims
  origdelims
  node
  previous
  next
  can_open
  can_close)

(cl-defstruct cmark--brackets
  node
  previous
  previousDelimiter
  index
  image
  active
  bracketAfter)

(cl-defstruct cmark--link
  destination
  title)

(cl-defstruct cmark-options
  smart
  time)

;; The InlineParser object.

(cl-defstruct cmark--InlineParser
  subject
  delimiters
  brackets
  pos
  refmap
  options)

(defun cmark--create-InlineParser (&optional options)
  (make-cmark--InlineParser
   :subject ""
   :delimiters nil ;; used by handleDelim method
   :brackets nil
   :pos 0
   :refmap (make-hash-table :test 'equal)
   :options (or options (make-cmark-options))))


;; These are methods of an InlineParser object, defined below.
;; An InlineParser keeps track of a subject (a string to be
;; parsed) and a position in that subject.

(defun cmark--InlineParser-match (this re)
  "If re matches at current position in the subject, advance
position in subject and return the match; otherwise return nil."
  (let* ((subject (cmark--InlineParser-subject this))
         (pos (cmark--InlineParser-pos this))
         (s (substring subject pos)))
    (when (cmark--string-match re s)
      (setf (cmark--InlineParser-pos this)
            (+ pos (match-end 0)))
      (match-string 0 s))))

(defun cmark--InlineParser-peek (this)
  (let ((pos (cmark--InlineParser-pos this))
        (subject (cmark--InlineParser-subject this)))
    (if (< pos (length subject))
        (elt subject pos)
      -1)))

(defun cmark--InlineParser-spnl (this)
  "Parse zero or more space characters, including at most one newline"
  (cmark--InlineParser-match this cmark--reSpnl)
  t)

;; All of the parsers below try to match something at the current position
;; in the subject.  If they succeed in matching anything, they
;; return the inline matched, advancing the subject.

(defun cmark--InlineParser-parseBackticks (this block)
  "Attempt to parse backticks, adding either a backtick code span or a
literal sequence of backticks."
  (cl-block nil
    (let* ((ticks (cmark--InlineParser-match this cmark--reTicksHere))
           (subject (cmark--InlineParser-subject this))
           (pos (cmark--InlineParser-pos this))
           (afterOpenTicks pos)
           matched
           node
           contents)
      (when (null ticks)
        (cl-return nil))
      (while (setq matched (cmark--InlineParser-match this cmark--reTicks))
        (when (equal matched ticks)
          (setq pos (cmark--InlineParser-pos this))
          (setq node (cmark-create-Node "code"))
          (setq contents
                (cmark--replaceAll
                 "\n"
                 " "
                 (substring subject
                            afterOpenTicks
                            (- pos (length ticks)))))
          (if (and
               (> (length contents) 0)
               (cmark--string-match "[^ ]" contents)
               (eq ?\s (elt contents 0))
               (eq ?\s (elt contents (1- (length contents)))))
              (setf (cmark-Node-_literal node)
                    (substring contents 1 (max 1 (1- (length contents)))))
            (setf (cmark-Node-_literal node) contents))
          (cmark-Node-appendChild block node)
          (cl-return t)))
      ;; If we got here, we didn't match a closing backtick sequence.
      (setf (cmark--InlineParser-pos this) afterOpenTicks)
      (cmark-Node-appendChild block (cmark--text ticks))
      t)))

(defun cmark--InlineParser-parseBackslash (this block)
  "Parse a backslash-escaped special character, adding either the escaped
character, a hard line break (if the backslash is followed by a newline),
or a literal backslash to the block's children.  Assumes current character
is a backslash."
  (let ((subj (cmark--InlineParser-subject this))
        pos)
    (cl-incf (cmark--InlineParser-pos this))
    (setq pos (cmark--InlineParser-pos this))
    (cond
     ((eq cmark--C_NEWLINE (cmark--InlineParser-peek this))
      (cl-incf (cmark--InlineParser-pos this))
      (cmark-Node-appendChild block (cmark-create-Node "linebreak")))

     ((cmark--string-match cmark--reEscapable (cmark--charAt subj pos))
      (cmark-Node-appendChild block (cmark--text (cmark--charAt subj pos)))
      (cl-incf (cmark--InlineParser-pos this)))

     (t
      (cmark-Node-appendChild block (cmark--text "\\")))))
  t)

(defun cmark--InlineParser-parseAutolink (this block)
  "Attempt to parse an autolink (URL or email in pointy brackets)."
  (let (m
        dest
        node)
    (cond
     ((setq m (cmark--InlineParser-match this cmark--reEmailAutolink))
      (setq dest (substring m 1 (1- (length m))))
      (setq node (cmark-create-Node "link"))
      (setf (cmark-Node-_destination node)
            (cmark--normalizeURI (concat "mailto:" dest)))
      (setf (cmark-Node-_title node) "")
      (cmark-Node-appendChild node (cmark--text dest))
      (cmark-Node-appendChild block node)
      t)

     ((setq m (cmark--InlineParser-match this cmark--reAutolink))
      (setq dest (substring m 1 (1- (length m))))
      (setq node (cmark-create-Node "link"))
      (setf (cmark-Node-_destination node) (cmark--normalizeURI dest))
      (setf (cmark-Node-_title node) "")
      (cmark-Node-appendChild node (cmark--text dest))
      (cmark-Node-appendChild block node)
      t)

     (t
      nil))))

(defun cmark--InlineParser-parseHtmlTag (this block)
  (let ((m (cmark--InlineParser-match this cmark--reHtmlTag)))
    (if (null m)
        nil
      (let ((node (cmark-create-Node "html_inline")))
        (setf (cmark-Node-_literal node) m)
        (cmark-Node-appendChild block node)
        t))))

(defun cmark--InlineParser-scanDelims (this cc)
  "Scan a sequence of characters with code cc, and return information about
the number of delimiters and whether they are positioned such that
they can open and/or close emphasis or strong emphasis.  A utility
function for strong/emph parsing."
  (let ((subject (cmark--InlineParser-subject this))
        (numdelims 0)
        char_before
        char_after
        cc_after
        (startpos (cmark--InlineParser-pos this))
        left_flanking
        right_flanking
        can_open
        can_close
        after_is_whitespace
        after_is_punctuation
        before_is_whitespace
        before_is_punctuation)
    (if (or (eq cc cmark--C_SINGLEQUOTE) (eq cc cmark--C_DOUBLEQUOTE))
        (progn (setq numdelims (1+ numdelims))
               (cl-incf (cmark--InlineParser-pos this)))
      (while (eq (cmark--InlineParser-peek this) cc)
        (setq numdelims (1+ numdelims))
        (cl-incf (cmark--InlineParser-pos this))))

    (if (zerop numdelims)
        nil
      (setq char_before (if (zerop startpos)
                            "\n"
                          (cmark--charAt subject (1- startpos))))
      (setq cc_after (cmark--InlineParser-peek this))
      (if (eq cc_after -1)
          (setq char_after "\n")
        (setq char_after (cmark--fromCodePoint cc_after)))
      (setq after_is_whitespace (cmark--string-match
                                 cmark--reUnicodeWhitespaceChar
                                 char_after))
      (setq after_is_punctuation (cmark--string-match
                                  cmark--rePunctuation
                                  char_after))
      (setq before_is_whitespace (cmark--string-match
                                  cmark--reUnicodeWhitespaceChar
                                  char_before))
      (setq before_is_punctuation (cmark--string-match
                                   cmark--rePunctuation
                                   char_before))
      (setq left_flanking (and (not after_is_whitespace)
                               (or (not after_is_punctuation)
                                   before_is_whitespace
                                   before_is_punctuation)))
      (setq right_flanking (and (not before_is_whitespace)
                                (or (not before_is_punctuation)
                                    after_is_whitespace
                                    after_is_punctuation)))
      (cond
       ((eq cc cmark--C_UNDERSCORE)
        (setq can_open (and left_flanking
                            (or (not right_flanking)
                                before_is_punctuation)))
        (setq can_close (and right_flanking
                                 (or (not left_flanking)
                                     before_is_punctuation))))

       ((or (eq cc cmark--C_SINGLEQUOTE)
            (eq cc cmark--C_DOUBLEQUOTE))
        (setq can_open (and left_flanking (not right_flanking)))
        (setq can_close right_flanking))

       (t
        (setq can_open left_flanking)
        (setq can_close right_flanking)))
      (setf (cmark--InlineParser-pos this) startpos)
      (make-cmark--delimiters :numdelims numdelims
                              :can_open can_open
                              :can_close can_close))))


(defun cmark--InlineParser-handleDelim (this cc block)
  "Handle a delimiter marker for emphasis or a quote."
  (let ((subject (cmark--InlineParser-subject this))
        (res (cmark--InlineParser-scanDelims this cc))
        numdelims
        startpos
        contents
        node)
    (if (null res)
        nil
      (setq numdelims (cmark--delimiters-numdelims res))
      (setq startpos (cmark--InlineParser-pos this))
      (cl-incf (cmark--InlineParser-pos this) numdelims)
      (cond
       ((eq cc cmark--C_SINGLEQUOTE)
        (setq contents "\u2019"))
       ((eq cc cmark--C_DOUBLEQUOTE)
        (setq contents "\u201C"))
       (t
        (setq contents (substring subject
                                  startpos
                                  (cmark--InlineParser-pos this)))))
      (setq node (cmark--text contents))
      (cmark-Node-appendChild block node)
      ;; Add entry to stack for this opener
      (when (and
             (or (cmark--delimiters-can_open res)
                 (cmark--delimiters-can_close res))
             (or (cmark-options-smart (cmark--InlineParser-options this))
                 (and (not (eq cc cmark--C_SINGLEQUOTE))
                      (not (eq cc cmark--C_DOUBLEQUOTE)))))
        (setf (cmark--InlineParser-delimiters this)
              (make-cmark--delimiters
               :cc cc
               :numdelims numdelims
               :origdelims numdelims
               :node node
               :previous (cmark--InlineParser-delimiters this)
               :next nil
               :can_open (cmark--delimiters-can_open res)
               :can_close (cmark--delimiters-can_close res)))
        (when (not
               (null
                (cmark--delimiters-previous
                 (cmark--InlineParser-delimiters this))))
          (setf (cmark--delimiters-next
                 (cmark--delimiters-previous
                  (cmark--InlineParser-delimiters this)))
                (cmark--InlineParser-delimiters this)))))
    t))

(defun cmark--InlineParser-removeDelimiter (this delim)
  (when (not (null (cmark--delimiters-previous delim)))
    (setf (cmark--delimiters-next (cmark--delimiters-previous delim))
          (cmark--delimiters-next delim)))
  (if (null (cmark--delimiters-next delim))
      (setf (cmark--InlineParser-delimiters this)
            (cmark--delimiters-previous delim))
    (setf (cmark--delimiters-previous (cmark--delimiters-next delim))
          (cmark--delimiters-previous delim))))


(defun cmark--InlineParser-removeDelimitersBetween (bottom top)
  (when (not (eq (cmark--delimiters-next bottom) top))
    (setf (cmark--delimiters-next bottom) top)
    (setf (cmark--delimiters-previous top) bottom)))

(defun cmark--InlineParser-processEmphasis (this stack_bottom)
  (let (opener
        closer
        old_closer
        opener_inl
        closer_inl
        tempstack
        use_delims
        tmp
        next
        opener_found
        (openers_bottom (make-hash-table :test 'equal))
        (odd_match nil)
        closercc
        emph)
    (puthash cmark--C_UNDERSCORE stack_bottom openers_bottom)
    (puthash cmark--C_ASTERISK stack_bottom openers_bottom)
    (puthash cmark--C_SINGLEQUOTE stack_bottom openers_bottom)
    (puthash cmark--C_DOUBLEQUOTE stack_bottom openers_bottom)

    ;; find first closer above stack_bottom:
    (setq closer (cmark--InlineParser-delimiters this))
    (while (and (not (null closer))
                (not (eq (cmark--delimiters-previous closer)
                         stack_bottom)))
      (setq closer (cmark--delimiters-previous closer)))

    ;; move forward, looking for closers, and handling each
    (while (not (null closer))
      (setq closercc (cmark--delimiters-cc closer))
      (if (not (cmark--delimiters-can_close closer))
          (setq closer (cmark--delimiters-next closer))
        ;; found emphasis closer. now look back for first matching opener:
        (setq opener (cmark--delimiters-previous closer))
        (setq opener_found nil)
        (cl-block find-opener
          (while (and (not (null opener))
                      (not (eq opener stack_bottom))
                      (not (eq opener (gethash closercc openers_bottom))))
            (setq odd_match
                  (and (or (cmark--delimiters-can_open closer)
                           (cmark--delimiters-can_close opener))
                       (not (zerop (% (cmark--delimiters-origdelims closer) 3)))
                       (zerop (% (+
                                  (cmark--delimiters-origdelims opener)
                                  (cmark--delimiters-origdelims closer))
                                 3))))
            (when (and (eq (cmark--delimiters-cc opener)
                           (cmark--delimiters-cc closer))
                       (cmark--delimiters-can_open opener)
                       (not odd_match))
              (setq opener_found t)
              (cl-return-from find-opener))
            (setq opener (cmark--delimiters-previous opener))))
        (setq old_closer closer)
        (cond
         ((or (eq closercc cmark--C_ASTERISK) (eq closercc cmark--C_UNDERSCORE))
          (if (not opener_found)
              (setq closer (cmark--delimiters-next closer))
            ;; calculate actual number of delimiters used from closer
            (setq use_delims
                  (if (and
                       (>= (cmark--delimiters-numdelims closer) 2)
                       (>= (cmark--delimiters-numdelims opener) 2))
                      2
                    1))

            (setq opener_inl (cmark--delimiters-node opener))
            (setq closer_inl (cmark--delimiters-node closer))

            ;; remove used delimiters from stack elts and inlines
            (cl-incf (cmark--delimiters-numdelims opener) (- use_delims))
            (cl-incf (cmark--delimiters-numdelims closer) (- use_delims))

            (setf (cmark-Node-_literal opener_inl)
                  (substring
                   (cmark-Node-_literal opener_inl)
                   0
                   (- (length (cmark-Node-_literal opener_inl))
                      use_delims)))
            (setf (cmark-Node-_literal closer_inl)
                  (substring
                   (cmark-Node-_literal closer_inl)
                   0
                   (- (length (cmark-Node-_literal closer_inl))
                      use_delims)))

            ;; build contents for new emph element
            (setq emph
                  (cmark-create-Node
                   (if (eq use_delims 1) "emph" "strong")))

            (setq tmp (cmark-Node-_next opener_inl))
            (while (and tmp (not (eq tmp closer_inl)))
              (setq next (cmark-Node-_next tmp))
              (cmark-Node-unlink tmp)
              (cmark-Node-appendChild emph tmp)
              (setq tmp next))

            (cmark-Node-insertAfter opener_inl emph)

            ;; remove elts between opener and closer in delimiters stack
            (cmark--InlineParser-removeDelimitersBetween opener closer)

            ;; if opener has 0 delims, remove it and the inline
            (when (zerop (cmark--delimiters-numdelims opener))
              (cmark-Node-unlink opener_inl)
              (cmark--InlineParser-removeDelimiter this opener))

            (when (zerop (cmark--delimiters-numdelims closer))
              (cmark-Node-unlink closer_inl)
              (setq tempstack (cmark--delimiters-next closer))
              (cmark--InlineParser-removeDelimiter this closer)
              (setq closer tempstack))))

         ((eq closercc cmark--C_SINGLEQUOTE)
          (setf (cmark-Node-_literal (cmark--delimiters-node closer))
                "\u2019")
          (when opener_found
            (setf (cmark-Node-_literal (cmark--delimiters-node opener))
                  "\u2018"))
          (setq closer (cmark--delimiters-next closer)))

         ((eq closercc cmark--C_DOUBLEQUOTE)
          (setf (cmark-Node-_literal (cmark--delimiters-node closer))
                "\u201D")
          (when opener_found
            (setf (cmark-Node-_literal (cmark--delimiters-node opener))
                  "\u201C"))
          (setq closer (cmark--delimiters-next closer))))
        (when (and (not opener_found) (not odd_match))
          ;; Set lower bound for future searches for openers:
          (puthash closercc
                   (cmark--delimiters-previous old_closer)
                   openers_bottom)
          (when (not (cmark--delimiters-can_open old_closer))
            ;; We can remove a closer that can't be an opener,
            ;; once we've seen there's no matching opener:
            (cmark--InlineParser-removeDelimiter this old_closer)))))
    ;; remove all delimiters
    (while (and (not (null (cmark--InlineParser-delimiters this)))
                (not (eq (cmark--InlineParser-delimiters this)
                         stack_bottom)))
      (cmark--InlineParser-removeDelimiter
       this
       (cmark--InlineParser-delimiters this)))))

(defun cmark--InlineParser-parseLinkTitle (this)
  "Attempt to parse link title (sans quotes), returning the string
or null if no match."
  (let ((title (cmark--InlineParser-match this cmark--reLinkTitle)))
    (if (null title)
        nil
      (cmark--unescapeString
       (substring title 1 (1- (length title)))))))

(defun cmark--InlineParser-parseLinkDestination (this)
  "Attempt to parse link destination, returning the string or
null if no match."
  (let ((res (cmark--InlineParser-match this cmark--reLinkDestinationBraces))
        savepos
        openparens
        c)
    (if (null res)
        (if (eq (cmark--InlineParser-peek this) cmark--C_LESSTHAN)
            nil
          ;; TODO handrolled parser; res should be null or the string
          (setq savepos (cmark--InlineParser-pos this))
          (setq openparens 0)

          (cl-block while
            (while (not (eq (setq c (cmark--InlineParser-peek this)) -1))
              (cond
               ((and (eq c cmark--C_BACKSLASH)
                     (cmark--string-match
                      cmark--reEscapable
                      (cmark--charAt
                       (cmark--InlineParser-subject this)
                       (1+ (cmark--InlineParser-pos this)))))
                (cl-incf (cmark--InlineParser-pos this))
                (when (not (eq (cmark--InlineParser-peek this) -1))
                  (cl-incf (cmark--InlineParser-pos this))))

               ((eq c cmark--C_OPEN_PAREN)
                (cl-incf (cmark--InlineParser-pos this))
                (setq openparens (1+ openparens)))

               ((eq c cmark--C_CLOSE_PAREN)
                (if (< openparens 1)
                    (cl-return-from while)
                  (cl-incf (cmark--InlineParser-pos this))
                  (setq openparens (1- openparens))))
               ((not (null (cmark--string-match
                            cmark--reWhitespaceChar
                            (cmark--fromCodePoint c))))
                (cl-return-from while))
               (t
                (cl-incf (cmark--InlineParser-pos this))))))
          (cond
           ((and (eq (cmark--InlineParser-pos this) savepos)
                 (not (eq c cmark--C_CLOSE_PAREN)))
            nil)

           ((not (zerop openparens))
            nil)

           (t
            (setq res (substring
                       (cmark--InlineParser-subject this)
                       savepos
                       (cmark--InlineParser-pos this)))
            (cmark--normalizeURI (cmark--unescapeString res)))))
      (cmark--normalizeURI (cmark--unescapeString (substring
                                                   res
                                                   1
                                                   (1- (length res))))))))

(defun cmark--InlineParser-parseLinkLabel (this)
  "Attempt to parse a link label, returning number of characters parsed."
  (let ((m (cmark--InlineParser-match this cmark--reLinkLabel)))
    (if (or (null m) (> (length m) 1001))
        0
      (length m))))

(defun cmark--InlineParser-parseOpenBracket (this block)
  "Add open bracket to delimiter stack and add a text node to block's children."
  (let ((startpos (cmark--InlineParser-pos this))
        (node (cmark--text "[")))
    (cl-incf (cmark--InlineParser-pos this))

    (cmark-Node-appendChild block node)

    ;; Add entry to stack for this opener
    (cmark--InlineParser-addBracket this node startpos nil)
    t))

(defun cmark--InlineParser-parseBang (this block)
  "IF next character is [, and ! delimiter to delimiter stack and
add a text node to block's children.  Otherwise just add a text node."
  (let ((startpos (cmark--InlineParser-pos this))
        node)
    (cl-incf (cmark--InlineParser-pos this))

    (if (eq (cmark--InlineParser-peek this) cmark--C_OPEN_BRACKET)
        (progn
          (cl-incf (cmark--InlineParser-pos this))

          (setq node (cmark--text "!["))
          (cmark-Node-appendChild block node)

          ;; Add entry to stack for this opener
          (cmark--InlineParser-addBracket this node (1+ startpos) t))
      (cmark-Node-appendChild block (cmark--text "!")))
    t))

(defun cmark--InlineParser-parseCloseBracket (this block)
  "Try to match close bracket against an opening in the delimiter
stack.  Add either a link or image, or a plain [ character,
to block's children.  If there is a matching delimiter,
remove it from the delimiter stack."
  (cl-block nil
    (let (startpos
          is_image
          dest
          title
          (matched nil)
          reflabel
          opener
          savepos
          beforelabel
          n
          link
          node
          tmp
          next)
      (cl-incf (cmark--InlineParser-pos this))
      (setq startpos (cmark--InlineParser-pos this))

      ;; get last [ or ![
      (setq opener (cmark--InlineParser-brackets this))

      (when (null opener)
        ;; no matched opener, just return a literal
        (cmark-Node-appendChild block (cmark--text "]"))
        (cl-return t))

      (when (not (cmark--brackets-active opener))
        ;; no matched opener, just return a literal
        (cmark-Node-appendChild block (cmark--text "]"))
        ;; take opener off brackets stack
        (cmark--InlineParser-removeBracket this)
        (cl-return t))

      ;; If we got here, open is a potential opener
      (setq is_image (cmark--brackets-image opener))

      ;; Check to see if we have a link/image

      (setq savepos (cmark--InlineParser-pos this))

      ;; Inline link?
      (when (eq (cmark--InlineParser-peek this) cmark--C_OPEN_PAREN)
        (cl-incf (cmark--InlineParser-pos this))
        (if (and
             (cmark--InlineParser-spnl this)
             (not
              (null
               (setq dest (cmark--InlineParser-parseLinkDestination this))))
             (cmark--InlineParser-spnl this)
             ;; make sure there's a space before the title:
             (or (and (cmark--string-match
                       cmark--reWhitespaceChar
                       (cmark--charAt
                        (cmark--InlineParser-subject this)
                        (1- (cmark--InlineParser-pos this))))
                      (setq title (cmark--InlineParser-parseLinkTitle this)))
                 t)
             (cmark--InlineParser-spnl this)
             (eq (cmark--InlineParser-peek this) cmark--C_CLOSE_PAREN))
            (progn
              (cl-incf (cmark--InlineParser-pos this))
              (setq matched t))
          (setf (cmark--InlineParser-pos this) savepos)))

      (when (not matched)
        ;; Next, see if there's a link label
        (setq beforelabel (cmark--InlineParser-pos this))
        (setq n (cmark--InlineParser-parseLinkLabel this))
        (if (> n 2)
            (setq reflabel (substring (cmark--InlineParser-subject this)
                                      beforelabel
                                      (+ beforelabel n)))
          (when (not (cmark--brackets-bracketAfter opener))
            ;; Empty or missing second label means to use the first label as the reference.
            ;; The reference must not contain a bracket. If we know there's a bracket, we don't even bother checking it.
            (setq reflabel (substring
                            (cmark--InlineParser-subject this)
                            (cmark--brackets-index opener)
                            startpos))))
        (when (zerop n)
          ;; If shortcut reference link, rewind before spaces we skipped.
          (setf (cmark--InlineParser-pos this) savepos))

        (when reflabel
          ;; lookup rawlabel in refmap
          (setq link (gethash (cmark--normalizeReference reflabel)
                              (cmark--InlineParser-refmap this)))
          (when link
            (setq dest (cmark--link-destination link))
            (setq title (cmark--link-title link))
            (setq matched t))))

      (if matched
          (progn
            (setq node (cmark-create-Node (if is_image "image" "link")))
            (setf (cmark-Node-_destination node) dest)
            (setf (cmark-Node-_title node) (or title ""))

            (setq tmp (cmark-Node-_next (cmark--brackets-node opener)))
            (while tmp
              (setq next (cmark-Node-_next tmp))
              (cmark-Node-unlink tmp)
              (cmark-Node-appendChild node tmp)
              (setq tmp next))
            (cmark-Node-appendChild block node)
            (cmark--InlineParser-processEmphasis
             this
             (cmark--brackets-previousDelimiter opener))
            (cmark--InlineParser-removeBracket this)
            (cmark-Node-unlink (cmark--brackets-node opener))

            ;; We remove this bracket and processEmphasis will remove later delimiters.
            ;; Now, for a link, we also deactivate earlier link openers.
            ;; (no links in links)
            (when (not is_image)
              (setq opener (cmark--InlineParser-brackets this))
              (while (not (null opener))
                (when (not (cmark--brackets-image opener))
                  (setf (cmark--brackets-active opener) nil) ;; deactivate this opener
                  )
                (setq opener (cmark--brackets-previous opener))))
            t)
        ;; no match
        (cmark--InlineParser-removeBracket this) ;; remove this opener from stack
        (setf (cmark--InlineParser-pos this) startpos)
        (cmark-Node-appendChild block (cmark--text "]"))
        t))))

(defun cmark--InlineParser-addBracket (this node index image)
  (when (not (null (cmark--InlineParser-brackets this)))
    (setf (cmark--brackets-bracketAfter
           (cmark--InlineParser-brackets this))
          t))
  (setf (cmark--InlineParser-brackets this)
        (make-cmark--brackets
         :node node
         :previous (cmark--InlineParser-brackets this)
         :previousDelimiter (cmark--InlineParser-delimiters this)
         :index index
         :image image
         :active t)))

(defun cmark--InlineParser-removeBracket (this)
  (setf (cmark--InlineParser-brackets this)
        (cmark--brackets-previous (cmark--InlineParser-brackets this))))

(defun cmark--InlineParser-parseEntity (this block)
  "Attempt to parse an entity."
  (let ((m (cmark--InlineParser-match this cmark--reEntityHere)))
    (if m
        (progn
          (cmark-Node-appendChild block (cmark--text(cmark--decodeHTML m)))
          t)
      nil)))

(defun cmark--InlineParser-parseString (this block)
  "Parse a run of ordinary characters, or a single character with
a special meaning in markdown, as a plain string."
  (let ((m (cmark--InlineParser-match this cmark--reMain))
        enCount
        emCount)
    (if m
        (progn
          (if (cmark-options-smart (cmark--InlineParser-options this))
              (cmark-Node-appendChild
               block
               (cmark--text
                (cmark--replaceAll
                 cmark--reDash
                 (lambda (chars)
                   (setq enCount 0)
                   (setq emCount 0)
                   (cond
                    ((zerop (% (length chars) 3)) ;; If divisible by 3, use all em dashes
                     (setq emCount (/ (length chars) 3)))

                    ((zerop (% (length chars) 2)) ;; If divisible by 2, use all en dashes
                     (setq enCount (/ (length chars) 2)))

                    ((eq (% (length chars) 3) 2) ;; If 2 extra dashes, use en dash for last 2 em dashes for rest
                     (setq enCount 1)
                     (setq emCount (/ (- (length chars) 2) 3)))

                    (t ;; Use en dashes for last 4 hyphens em dashes for rest
                     (setq enCount 2)
                     (setq emCount (/ (- (length chars) 4) 3))))

                   (concat (cmark--repeat "\u2014" emCount)
                           (cmark--repeat "\u2013" enCount)))
                 (cmark--replaceAll cmark--reEllipses "\u2026" m))))
            (cmark-Node-appendChild block (cmark--text m)))
          t)
      nil)))

(defun cmark--InlineParser-parseNewline (this block)
  "Parse a newline.  If it was preceded by two spaces, return a hard
line break otherwise a soft line break."
  (cl-incf (cmark--InlineParser-pos this)) ;; assume we're at a \n
  ;; check previous node for trailing spaces
  (let* ((lastc (cmark-Node-_lastChild block))
         (lastc_literal (and lastc
                             (cmark-Node-_literal lastc)))
         hardbreak)
    (if (and lastc
             (equal (cmark-Node-type lastc) "text")
             (equal (cmark--charAt lastc_literal (1- (length lastc_literal)))
                    " "))
        (progn
          (setq hardbreak
                (equal (cmark--charAt lastc_literal
                                      (- (length lastc_literal) 2))
                       " "))

          (setf (cmark-Node-_literal lastc)
                (cmark--replaceAll cmark--reFinalSpace "" lastc_literal))
          (cmark-Node-appendChild
           block
           (cmark-create-Node (if hardbreak "linebreak" "softbreak"))))
      (cmark-Node-appendChild block (cmark-create-Node "softbreak")))
    (cmark--InlineParser-match this cmark--reInitialSpace) ;; gobble leading spaces in next line
    t))

(defun cmark--InlineParser-parseReference (this s refmap)
  "Attempt to parse a link reference, modifying refmap."
  (cl-block nil
    (setf (cmark--InlineParser-subject this) s)
    (setf (cmark--InlineParser-pos this) 0)

    (let (rawlabel
          dest
          title
          matchChars
          (startpos (cmark--InlineParser-pos this))
          beforetitle
          atLineEnd)
      ;; label:
      (setq matchChars (cmark--InlineParser-parseLinkLabel this))

      (if (zerop matchChars)
          (cl-return 0)
        (setq rawlabel (substring
                        (cmark--InlineParser-subject this)
                        0
                        matchChars)))

      ;; colon:
      (if (eq (cmark--InlineParser-peek this) cmark--C_COLON)
          (cl-incf (cmark--InlineParser-pos this))
        (setf (cmark--InlineParser-pos this) startpos)
        (cl-return 0))

      ;; link url
      (cmark--InlineParser-spnl this)

      (setq dest (cmark--InlineParser-parseLinkDestination this))
      (when (null dest)
        (setf (cmark--InlineParser-pos this) startpos)
        (cl-return 0))

      (setq beforetitle (cmark--InlineParser-pos this))
      (cmark--InlineParser-spnl this)
      (when (not (eq (cmark--InlineParser-pos this) beforetitle))
        (setq title (cmark--InlineParser-parseLinkTitle this)))
      (when (null title)
        (setq title "")
        ;; rewind before spaces
        (setf (cmark--InlineParser-pos this) beforetitle))

      ;; make sure we're at line end:
      (setq atLineEnd t)
      (when (null (cmark--InlineParser-match this cmark--reSpaceAtEndOfLine))
        (if (equal title "")
            (setq atLineEnd nil)
          ;; the potential title we found is not at the line end,
          ;; but it could still be a legal link reference if we
          ;; discard the title
          (setq title "")
          ;; rewind before spaces
          (setf (cmark--InlineParser-pos this) beforetitle)
          ;; and instead check if the link URL is at the line end
          (setq atLineEnd
                (not (null (cmark--InlineParser-match
                            this
                            cmark--reSpaceAtEndOfLine))))))

      (when (not atLineEnd)
        (setf (cmark--InlineParser-pos this) startpos)
        (cl-return 0))

      (let ((normlabel (cmark--normalizeReference rawlabel)))
        (when (equal normlabel "")
          ;; label must contain non-whitespace characters
          (setf (cmark--InlineParser-pos this) startpos)
          (cl-return 0))

        (when (null (gethash normlabel refmap))
          (puthash
           normlabel
           (make-cmark--link :destination dest :title title)
           refmap))
        (- (cmark--InlineParser-pos this) startpos)))))

(defun cmark--InlineParser-parseInline (this block)
  "Parse the next inline element in subject, advancing subject position.
On success, add the result to block's children and return t.
On failure, return nil."
  (let ((res nil)
        (c (cmark--InlineParser-peek this)))
    (if (eq c -1)
        nil
      (cond
       ((eq c cmark--C_NEWLINE)
        (setq res (cmark--InlineParser-parseNewline this block)))
       ((eq c cmark--C_BACKSLASH)
        (setq res (cmark--InlineParser-parseBackslash this block)))
       ((eq c cmark--C_BACKTICK)
        (setq res (cmark--InlineParser-parseBackticks this block)))
       ((memq c (list cmark--C_ASTERISK cmark--C_UNDERSCORE))
        (setq res (cmark--InlineParser-handleDelim this c block)))
       ((memq c (list cmark--C_SINGLEQUOTE cmark--C_DOUBLEQUOTE))
        (setq res (and (cmark-options-smart
                        (cmark--InlineParser-options this))
                       (cmark--InlineParser-handleDelim this c block))))
       ((eq c cmark--C_OPEN_BRACKET)
        (setq res (cmark--InlineParser-parseOpenBracket this block)))
       ((eq c cmark--C_BANG)
        (setq res (cmark--InlineParser-parseBang this block)))
       ((eq c cmark--C_CLOSE_BRACKET)
        (setq res (cmark--InlineParser-parseCloseBracket this block)))
       ((eq c cmark--C_LESSTHAN)
        (setq res (or (cmark--InlineParser-parseAutolink this block)
                      (cmark--InlineParser-parseHtmlTag this block))))
       ((eq c cmark--C_AMPERSAND)
        (setq res (cmark--InlineParser-parseEntity this block)))
       (t
        (setq res (cmark--InlineParser-parseString this block))))
      (when (not res)
        (cl-incf (cmark--InlineParser-pos this))
        (cmark-Node-appendChild block (cmark--text (cmark--fromCodePoint c))))
      t)))

(defun cmark--InlineParser-parseInlines (this block)
  "Parse string content in block into inline children,
using refmap to resolve references."
  (setf (cmark--InlineParser-subject this) (cmark--trim (cmark-Node-_string_content block)))
  (setf (cmark--InlineParser-pos this) 0)
  (setf (cmark--InlineParser-delimiters this) nil)
  (setf (cmark--InlineParser-brackets this) nil)
  (while (cmark--InlineParser-parseInline this block))
  (setf (cmark-Node-_string_content block) nil) ;; allow raw string to be garbage collected
  (cmark--InlineParser-processEmphasis this nil))

(defalias 'cmark--InlineParser-parse 'cmark--InlineParser-parseInlines)

(provide 'cmark-inlines)

;;; cmark-inlines.el ends here
