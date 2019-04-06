;;; cmark-blocks.el --- CommonMark parser. -*- lexical-binding: t -*-

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

;; A CommonMark parser.
;;
;; Usage:
;;
;; (let ((parser (cmark-create-Parser)))
;;   (cmark-Parser-parse parser source-string))

;;; Code

(require 'cmark-compat)

(require 'cmark-node)
(require 'cmark-common)

(defconst cmark--CODE_INDENT 4)

(defconst cmark--C_TAB 9)
(defconst cmark--C_NEWLINE 10)
(defconst cmark--C_GREATERTHAN 62)
(defconst cmark--C_LESSTHAN 60)
(defconst cmark--C_SPACE 32)
(defconst cmark--C_OPEN_BRACKET 91)

(require 'cmark-inlines)

(cl-defstruct cmark-Parser
  doc
  blocks
  blockStarts
  tip
  oldtip
  currentLine
  lineNumber
  offset
  column
  nextNonspace
  nextNonspaceColumn
  indent
  indented
  blank
  partiallyConsumedTab
  allClosed
  lastMatchedContainer
  refmap
  lastLineLength
  inlineParser
  options)

(cl-defstruct cmark--listMarker
  type
  tight
  bulletChar
  start
  delimiter
  padding
  markerOffset)

(defconst cmark--reHtmlBlockOpen
  (vector
   "." ;; dummy for 0
   (list (concat "\\`"
                 "<"
                 "\\(?:script\\|pre\\|style\\)"
                 "\\(?:"
                 cmark--SPACE
                 "\\|"
                 ">"
                 "\\|"
                 "$"
                 "\\)")
         :ignore-case)
   "\\`<!--"
   "\\`<[?]"
   "\\`<![A-Z]"
   "\\`<!\\[CDATA\\["
   (list (concat "\\`"
                 "<[/]?"
                 "\\(?:"
                 "address\\|article\\|aside\\|base\\|basefont\\|blockquote\\|body\\|caption\\|center\\|col\\|colgroup\\|dd\\|details\\|dialog\\|dir\\|div\\|dl\\|dt\\|fieldset\\|figcaption\\|figure\\|footer\\|form\\|frame\\|frameset\\|h[123456]\\|head\\|header\\|hr\\|html\\|iframe\\|legend\\|li\\|link\\|main\\|menu\\|menuitem\\|nav\\|noframes\\|ol\\|optgroup\\|option\\|p\\|param\\|section\\|source\\|title\\|summary\\|table\\|tbody\\|td\\|tfoot\\|th\\|thead\\|title\\|tr\\|track\\|ul"
                 "\\)"
                 "\\(?:"
                 cmark--SPACE
                 "\\|"
                 "[/]?[>]"
                 "\\|"
                 "$"
                 "\\)")
         :ignore-case)
   (list (concat "\\`"
                 "\\(?:"
                 cmark--OPENTAG
                 "\\|"
                 cmark--CLOSETAG
                 "\\)"
                 cmark--SPACE
                 "*"
                 "$")
         :ignore-case)))

(defconst cmark--reHtmlBlockClose
  (list
   "." ;; dummy for 0
   (list "<\\/\\(?:script\\|pre\\|style\\)>" :ignore-case)
   "-->"
   "\\?>"
   ">"
   "\\]\\]>"))

(defconst cmark--reThematicBreak
  (concat "\\`"
          "\\(?:"
          "\\(?:"
          "\\*[ \t]*"
          "\\)"
          "\\{3,\\}"
          "\\|"
          "\\(?:"
          "_[ \t]*"
          "\\)"
          "\\{3,\\}"
          "\\|"
          "\\(?:"
          "-[ \t]*"
          "\\)"
          "\\{3,\\}"
          "\\)"
          "[ \t]*"
          "$"))

(defconst cmark--reMaybeSpecial "\\`[#`~*+_=<>0-9-]")

(defconst cmark--reNonSpace "[^ \t\f\v\r\n]")

(defconst cmark--reBulletListMarker "\\`[*+-]")

(defconst cmark--reOrderedListMarker "\\`\\([[:digit:]]\\{1,9\\}\\)\\([.)]\\)")

(defconst cmark--reATXHeadingMarker "\\`#\\{1,6\\}\\(?:[ \t]+\\|$\\)")

(defconst cmark--reCodeFence "\\``\\{3,\\}\\|\\`~\\{3,\\}")

(defconst cmark--reClosingCodeFence "\\`\\(?:`\\{3,\\}\\|~\\{3,\\}\\)")

(defconst cmark--reSetextHeadingLine "\\`\\(?:=+\\|-+\\)[ \t]*$")

(defconst cmark--reLineEnding "\r\n\\|\n\\|\r")

(defun cmark--isBlank (s)
  "Returns true if string contains only space characters."
  (not (cmark--string-match cmark--reNonSpace s)))

(defun cmark--isSpaceOrTab (c)
  (or (eq c cmark--C_SPACE) (eq c cmark--C_TAB)))

(defun cmark--peek (ln pos)
  (if (< pos (length ln))
      (elt ln pos)
    -1))

;;; DOC PARSER

;; These are methods of a Parser object, defined below.

(defun cmark--endsWithBlankLine (block)
  "Returns t if block ends with a blank line, descending if needed
into lists and sublists."
  (cl-block nil
    (cl-block while
      (while block
        (when (cmark-Node-_lastLineBlank block)
          (cl-return t))
        (let ((type (cmark-Node-type block)))
          (if (and (not (cmark-Node-_lastLineChecked block))
                   (or (equal type "list") (equal type "item")))
              (progn
                (setf (cmark-Node-_lastLineChecked block) t)
                (setq block (cmark-Node-_lastChild block)))
            (setf (cmark-Node-_lastLineChecked block) t)
            (cl-return-from while)))))
    nil))

(defun cmark--Parser-addLine (this)
  "Add a line to the block at the tip.  We assume the tip
can accept lines -- that check should be done before calling this"
  (when (cmark-Parser-partiallyConsumedTab this)
    (cl-incf (cmark-Parser-offset this) 1) ;; skip over tab
    ;; add space characters:
    (let ((charsToTab (- 4 (% (cmark-Parser-column this) 4))))
      (cl-callf concat (cmark-Node-_string_content (cmark-Parser-tip this))
        (cmark--repeat " " charsToTab))))
  (cl-callf concat (cmark-Node-_string_content (cmark-Parser-tip this))
    (substring (cmark-Parser-currentLine this) (cmark-Parser-offset this))
    "\n"))

(defun cmark--Parser-addChild (this tag offset)
  "Add block of type tag as a child of the tip.  If the tip can't
accept children, close and finalize it and try its parent,
and so on til we find a block that can accept children."
  (while (not (funcall
               (cmark--block-canContain
                (gethash (cmark-Node-type (cmark-Parser-tip this))
                         (cmark-Parser-blocks this)))
               tag))
    (cmark--Parser-finalize this
                            (cmark-Parser-tip this)
                            (1- (cmark-Parser-lineNumber this))))
  (let* ((column_number (1+ offset)) ;; offset 0 = column 1
         (newBlock (cmark-create-Node
                    tag
                    (cons (cons (cmark-Parser-lineNumber this) column_number)
                          (cons 0 0)))))
    (setf (cmark-Node-_string_content newBlock) "")
    (cmark-Node-appendChild (cmark-Parser-tip this) newBlock)
    (setf (cmark-Parser-tip this) newBlock)
    newBlock))

(defun cmark--Parser-parseListMarker (parser container)
  "Parse a list marker and return data on the marker (type,
start, delimiter, bullet character, padding) or nil."
  (cl-block nil
    (let ((rest (substring (cmark-Parser-currentLine parser)
                           (cmark-Parser-nextNonspace parser)))
          nextc
          spacesStartCol
          spacesStartOffset
          (data (make-cmark--listMarker
                 :type nil
                 :tight t ;; lists are tight by default
                 :bulletChar nil
                 :start nil
                 :delimiter nil
                 :padding nil
                 :markerOffset (cmark-Parser-indent parser)))
          blank_item
          spaces_after_marker
          match-string)
      (when (>= (cmark-Parser-indent parser) 4)
        (cl-return nil))
      (cond
       ((cmark--string-match cmark--reBulletListMarker rest)
        (setf (cmark--listMarker-type data) "bullet")
        (setf (cmark--listMarker-bulletChar data)
              (cmark--charAt (match-string 0 rest) 0)))
       ((and (cmark--string-match cmark--reOrderedListMarker rest)
             (or (not (equal (cmark-Node-type container) "paragraph"))
                 (equal (match-string 1 rest) "1")))
        (setf (cmark--listMarker-type data) "ordered")
        (setf (cmark--listMarker-start data)
              (string-to-number (match-string 1 rest)))
        (setf (cmark--listMarker-delimiter data) (match-string 2 rest)))
       (t
        (cl-return nil)))
      (setq match-string (match-string 0 rest))
      ;; make sure we have spaces after
      (setq nextc (cmark--peek (cmark-Parser-currentLine parser)
                               (+ (cmark-Parser-nextNonspace parser)
                                  (length match-string))))
      (when (not (or (eq nextc -1)
                     (eq nextc cmark--C_TAB)
                     (eq nextc cmark--C_SPACE)))
        (cl-return nil))

      ;; if it interrupts paragraph, make sure first line isn't blank
      (when (and (equal (cmark-Node-type container) "paragraph")
                 (not (cmark--string-match
                       cmark--reNonSpace
                       (substring (cmark-Parser-currentLine parser)
                                  (+ (cmark-Parser-nextNonspace parser)
                                     (length match-string))))))
        (cl-return nil))

      ;; we've got a match! advance offset and calculate padding
      (cmark--Parser-advanceNextNonspace parser) ;; to start of marker
      (cmark--Parser-advanceOffset parser (length match-string) t) ;; to end of marker
      (setq spacesStartCol (cmark-Parser-column parser))
      (setq spacesStartOffset (cmark-Parser-offset parser))
      (while (progn
               (cmark--Parser-advanceOffset parser 1 t)
               (setq nextc (cmark--peek (cmark-Parser-currentLine parser)
                                        (cmark-Parser-offset parser)))
               (and (< (- (cmark-Parser-column parser) spacesStartCol) 5)
                    (cmark--isSpaceOrTab nextc))))
      (setq blank_item (eq (cmark--peek (cmark-Parser-currentLine parser)
                                        (cmark-Parser-offset parser))
                           -1))
      (setq spaces_after_marker (- (cmark-Parser-column parser)
                                   spacesStartCol))
      (if (or (>= spaces_after_marker 5)
              (< spaces_after_marker 1)
              blank_item)
          (progn
            (setf (cmark--listMarker-padding data) (1+ (length match-string)))
            (setf (cmark-Parser-column parser) spacesStartCol)
            (setf (cmark-Parser-offset parser) spacesStartOffset)
            (when (cmark--isSpaceOrTab
                   (cmark--peek (cmark-Parser-currentLine parser)
                                (cmark-Parser-offset parser)))
              (cmark--Parser-advanceOffset parser 1 t)))
        (setf (cmark--listMarker-padding data)
              (+ (length match-string) spaces_after_marker)))
      data)))

(defun cmark--listsMatch (list_data item_data)
  "Returns t if the two list items are of the same type,
with the same delimiter and bullet character.  This is used
in agglomerating list items into lists."
  (and (equal (cmark--listMarker-type list_data)
              (cmark--listMarker-type item_data))
       (equal (cmark--listMarker-delimiter list_data)
              (cmark--listMarker-delimiter item_data))
       (equal (cmark--listMarker-bulletChar list_data)
              (cmark--listMarker-bulletChar item_data))))

(defun cmark--Parser-closeUnmatchedBlocks (this)
  "Finalize and close any unmatched blocks."
  (when (not (cmark-Parser-allClosed this))
    ;; finalize any blocks not matched
    (while (not (eq (cmark-Parser-oldtip this)
                    (cmark-Parser-lastMatchedContainer this)))
      (let ((parent (cmark-Node-_parent (cmark-Parser-oldtip this))))
        (cmark--Parser-finalize this
                                (cmark-Parser-oldtip this)
                                (1- (cmark-Parser-lineNumber this)))
        (setf (cmark-Parser-oldtip this) parent)))
    (setf (cmark-Parser-allClosed this) t)))

(cl-defstruct cmark--block
  continue
  finalize
  canContain
  acceptsLines)

(defconst cmark--blocks
  (let ((blocks (make-hash-table :test 'equal)))
    (puthash
     "document"
     (make-cmark--block
      :continue (lambda (_parser _container) 0)
      :finalize (lambda (_parser _block))
      :canContain (lambda (type) (not (equal type "item")))
      :acceptsLines nil)
     blocks)

    (puthash
     "list"
     (make-cmark--block
      :continue (lambda (_parser _container) 0)
      :finalize
      (lambda (_parser block)
        (let ((item (cmark-Node-_firstChild block))
              subitem)
          (cl-block nil
            (while item
              ;; check for non-final list item ending with blank line:
              (when (and (cmark--endsWithBlankLine item)
                         (cmark-Node-_next item))
                (setf (cmark--listMarker-tight (cmark-Node-_listData block))
                      nil)
                (cl-return))
              ;; recurse into children of list item, to see if there are
              ;; spaces between any of them:
              (setq subitem (cmark-Node-_firstChild item))

              (cl-block while
                (while subitem
                  (when (and (cmark--endsWithBlankLine subitem)
                             (or (cmark-Node-_next item)
                                 (cmark-Node-_next subitem)))
                    (setf (cmark--listMarker-tight
                           (cmark-Node-_listData block))
                          nil)
                    (cl-return-from while))
                  (setq subitem (cmark-Node-_next subitem))))
              (setq item (cmark-Node-_next item))))))
      :canContain (lambda (type) (equal type "item"))
      :acceptsLines nil)
     blocks)

    (puthash
     "block_quote"
     (make-cmark--block
      :continue
      (lambda (parser _container)
        (let ((ln (cmark-Parser-currentLine parser)))
          (if (and (not (cmark-Parser-indented parser))
                   (eq (cmark--peek ln (cmark-Parser-nextNonspace parser))
                       cmark--C_GREATERTHAN))
              (progn
                (cmark--Parser-advanceNextNonspace parser)
                (cmark--Parser-advanceOffset parser 1 nil)
                (when (cmark--isSpaceOrTab
                       (cmark--peek ln (cmark-Parser-offset parser)))
                  (cmark--Parser-advanceOffset parser 1 t))
                0)
            1)))
      :finalize (lambda (_parser _block))
      :canContain (lambda (type) (not (equal type "item")))
      :acceptsLines nil)
     blocks)

    (puthash
     "item"
     (make-cmark--block
      :continue
      (lambda (parser container)
        (cond
         ((cmark-Parser-blank parser)
          (if (null (cmark-Node-_firstChild container))
              ;; Blank line after empty list item
              1
            (cmark--Parser-advanceNextNonspace parser)
            0))

         ((>= (cmark-Parser-indent parser)
              (+ (cmark--listMarker-markerOffset
                  (cmark-Node-_listData container))
                 (cmark--listMarker-padding
                  (cmark-Node-_listData container))))
          (cmark--Parser-advanceOffset
           parser
           (+ (cmark--listMarker-markerOffset
               (cmark-Node-_listData container))
              (cmark--listMarker-padding
               (cmark-Node-_listData container)))
           t)
          0)

         (t
          1)))
      :finalize (lambda (_parser _block))
      :canContain (lambda (type) (not (equal type "item")))
      :acceptsLines nil)
     blocks)

    (puthash
     "heading"
     (make-cmark--block
      :continue
      (lambda (_parser _container)
        ;; a heading can never container > 1 line, so fail to match:
        1)
      :finalize (lambda (_parser _block))
      :canContain (lambda () nil)
      :acceptsLines nil)
     blocks)

    (puthash
     "thematic_break"
     (make-cmark--block
      :continue
      (lambda (_parser _container)
        ;; a thematic break can never container > 1 line, so fail to match:
        1)
      :finalize (lambda (_parser _block))
      :canContain (lambda () nil)
      :acceptsLines nil)
     blocks)

    (puthash
     "code_block"
     (make-cmark--block
      :continue
      (lambda (parser container)
        (let ((ln (cmark-Parser-currentLine parser))
              (indent (cmark-Parser-indent parser))
              match
              i
              subln)
          (cond
           ((cmark-Node-_isFenced container)
            ;; fenced
            (setq subln (substring
                         ln
                         (cmark-Parser-nextNonspace parser)))
            (setq match
                  (and (<= indent 3)
                       (equal (cmark--charAt
                               ln
                               (cmark-Parser-nextNonspace parser))
                              (cmark-Node-_fenceChar container))
                       (cmark--string-match cmark--reClosingCodeFence subln)
                       ;; Since Emacs Lisp doesn't support positive lookahead,
                       ;; check it manually.
                       (save-match-data
                         (cmark--string-match
                          (concat cmark--reClosingCodeFence " *\\'")
                          subln))))
            (if (and match
                     (>= (length (match-string 0 subln))
                         (cmark-Node-_fenceLength container)))
                (progn
                  ;; closing fence - we're at end of line, so we can return
                  (cmark--Parser-finalize parser
                                          container
                                          (cmark-Parser-lineNumber parser))
                  2)
              ;; skip optional spaces of fence offset
              (setq i (cmark-Node-_fenceOffset container))
              (while (and (> i 0)
                          (cmark--isSpaceOrTab
                           (cmark--peek ln (cmark-Parser-offset parser))))
                (cmark--Parser-advanceOffset parser 1 t)
                (setq i (1- i)))
              0))

           ;; indented

           ((>= indent cmark--CODE_INDENT)
            (cmark--Parser-advanceOffset parser cmark--CODE_INDENT t)
            0)

           ((cmark-Parser-blank parser)
            (cmark--Parser-advanceNextNonspace parser)
            0)

           (t
            1))))
      :finalize
      (lambda (_parser block)
        (if (cmark-Node-_isFenced block)
            (progn ;; fenced
              ;; first line becomes info string
              (let* ((content (cmark-Node-_string_content block))
                     (newlinePos (cl-search "\n" content))
                     (firstLine (substring content 0 newlinePos))
                     (rest (substring content (1+ newlinePos))))
                (setf (cmark-Node-info block)
                      (cmark--unescapeString (cmark--trim firstLine)))
                (setf (cmark-Node-_literal block) rest)))
          ;; indented
          (setf (cmark-Node-_literal block)
                (cmark--replaceAll "\\(\n *\\)+\\'"
                                   "\n"
                                   (cmark-Node-_string_content block))))
        (setf (cmark-Node-_string_content block) nil)) ;; allow GC
      :canContain (lambda () nil)
      :acceptsLines t)
     blocks)

    (puthash
     "html_block"
     (make-cmark--block
      :continue
      (lambda (parser container)
        (if (and (cmark-Parser-blank parser)
                 (or (eq (cmark-Node-_htmlBlockType container) 6)
                     (eq (cmark-Node-_htmlBlockType container) 7)))
            1
          0))
      :finalize
      (lambda (_parser block)
        (setf (cmark-Node-_literal block)
              (cmark--replaceAll "\\(\n *\\)+\\'"
                                 ""
                                 (cmark-Node-_string_content block)))
        (setf (cmark-Node-_string_content block) nil)) ;; allow GC
      :canContain (lambda () nil)
      :acceptsLines t)
     blocks)

    (puthash
     "paragraph"
     (make-cmark--block
      :continue
      (lambda (parser _container)
        (if (cmark-Parser-blank parser) 1 0))
      :finalize
      (lambda (parser block)
        (let (pos
              (hasReferenceDefs nil))
          ;; try parsing the beginning as link reference definitions.
          (while (and
                  (eq (cmark--peek (cmark-Node-_string_content block) 0)
                      cmark--C_OPEN_BRACKET)
                  (not (zerop (setq pos
                                    (cmark--InlineParser-parseReference
                                     (cmark-Parser-inlineParser parser)
                                     (cmark-Node-_string_content block)
                                     (cmark-Parser-refmap parser))))))
            (setf (cmark-Node-_string_content block)
                  (substring (cmark-Node-_string_content block) pos))
            (setq hasReferenceDefs t))
          (when (and hasReferenceDefs
                     (cmark--isBlank (cmark-Node-_string_content block)))
            (cmark-Node-unlink block))))
      :canContain (lambda (_type) nil)
      :acceptsLines t)
     blocks)
    blocks)
  "\"finalize\" is run when the block is closed.
\"continue\" is run to check whether the block is continuing
at a certain line and offset (e.g. whether a block quote
contains a `>`.  It returns 0 for matched, 1 for not matched,
and 2 for \"we've dealt with this line completely, go to next.\"")

(defconst cmark--blockStarts
  (vector
   ;; block quote
   (lambda (parser _container)
     (if (and (not (cmark-Parser-indented parser))
              (eq (cmark--peek (cmark-Parser-currentLine parser)
                               (cmark-Parser-nextNonspace parser))
                  cmark--C_GREATERTHAN))
         (progn
           (cmark--Parser-advanceNextNonspace parser)
           (cmark--Parser-advanceOffset parser 1 nil)
           ;; optional following space
           (when (cmark--isSpaceOrTab (cmark--peek
                                       (cmark-Parser-currentLine parser)
                                       (cmark-Parser-offset parser)))
             (cmark--Parser-advanceOffset parser 1 t))
           (cmark--Parser-closeUnmatchedBlocks parser)
           (cmark--Parser-addChild parser
                                   "block_quote"
                                   (cmark-Parser-nextNonspace parser))
           1)
       0))

   ;; ATX heading
   (lambda (parser _container)
     (let (current-line-after-spaces)
       (if (and (not (cmark-Parser-indented parser))
                (cmark--string-match
                 cmark--reATXHeadingMarker
                 (setq current-line-after-spaces
                       (substring (cmark-Parser-currentLine parser)
                                  (cmark-Parser-nextNonspace parser)))))
           (let ((match-string (match-string
                                0
                                current-line-after-spaces))
                 container)
             (cmark--Parser-advanceNextNonspace parser)
             (cmark--Parser-advanceOffset parser (length match-string) nil)
             (cmark--Parser-closeUnmatchedBlocks parser)
             (setq container (cmark--Parser-addChild
                              parser
                              "heading"
                              (cmark-Parser-nextNonspace parser)))
             (setf (cmark-Node-level container)
                   (length (cmark--trim match-string))) ;; number of #s
             ;; remove trailing ###s:
             (setf (cmark-Node-_string_content container)
                   (cmark--replaceAll
                    "[ \t]+#+[ \t]*\\'"
                    ""
                    (cmark--replaceAll
                     "\\`[ \t]*#+[ \t]*\\'"
                     ""
                     (substring (cmark-Parser-currentLine parser)
                                (cmark-Parser-offset parser)))))
             (cmark--Parser-advanceOffset
              parser
              (- (length (cmark-Parser-currentLine parser))
                 (cmark-Parser-offset parser))
              nil)
             2)
         0)))

   ;; Fenced code block
   (lambda (parser _container)
     (let (current-line-after-spaces)
       (if (and (not (cmark-Parser-indented parser))
                (cmark--string-match
                 cmark--reCodeFence
                 (setq current-line-after-spaces
                       (substring (cmark-Parser-currentLine parser)
                                  (cmark-Parser-nextNonspace parser))))
                ;; Since Emacs Lisp doesn't support negative lookahead,
                ;; check it manually.
                (save-match-data
                  (not (cmark--string-match
                        "`\\{3,\\}[^`].*`"
                        current-line-after-spaces))))
           (progn
             (let* ((match-string (match-string 0 current-line-after-spaces))
                    (fenceLength (length match-string))
                    container)
               (cmark--Parser-closeUnmatchedBlocks parser)
               (setq container
                     (cmark--Parser-addChild
                      parser
                      "code_block"
                      (cmark-Parser-nextNonspace parser)))
               (setf (cmark-Node-_isFenced container) t)
               (setf (cmark-Node-_fenceLength container) fenceLength)
               (setf (cmark-Node-_fenceChar container)
                     (cmark--charAt match-string 0))
               (setf (cmark-Node-_fenceOffset container)
                     (cmark-Parser-indent parser))
               (cmark--Parser-advanceNextNonspace parser)
               (cmark--Parser-advanceOffset parser fenceLength nil)
               2))
         0)))

   ;; HTML block
   (lambda (parser container)
     (cl-block nil
       (if (and (not (cmark-Parser-indented parser))
                (eq (cmark--peek (cmark-Parser-currentLine parser)
                                 (cmark-Parser-nextNonspace parser))
                    cmark--C_LESSTHAN))
           (progn
             (let ((s (substring (cmark-Parser-currentLine parser)
                                 (cmark-Parser-nextNonspace parser)))
                   (blockType 1)
                   b)
               (while (<= blockType 7)
                 (when (and (cmark--string-match
                             (elt cmark--reHtmlBlockOpen blockType)
                             s)
                            (or (< blockType 7)
                                (not (equal (cmark-Node-type container)
                                            "paragraph"))))
                   (cmark--Parser-closeUnmatchedBlocks parser)
                   ;; We don't adjust (cmark-Parser-offset parser)
                   ;; spaces are part of the HTML block:
                   (setq b (cmark--Parser-addChild
                            parser
                            "html_block"
                            (cmark-Parser-offset parser)))
                   (setf (cmark-Node-_htmlBlockType b) blockType)
                   (cl-return 2))
                 (setq blockType (1+ blockType)))
               0))
         0)))

   ;; Setext heading
   (lambda (parser container)
     (let (current-line-after-spaces)
       (if (and (not (cmark-Parser-indented parser))
                (equal (cmark-Node-type container) "paragraph")
                (cmark--string-match
                 cmark--reSetextHeadingLine
                 (setq current-line-after-spaces
                       (substring (cmark-Parser-currentLine parser)
                                  (cmark-Parser-nextNonspace parser)))))
           (progn
             (let ((match-string (match-string 0 current-line-after-spaces))
                   heading)
               (cmark--Parser-closeUnmatchedBlocks parser)
               (setq heading (cmark-create-Node
                              "heading"
                              (cmark-Node-sourcepos container)))
               (setf (cmark-Node-level heading)
                     (if (equal (cmark--charAt match-string 0) "=") 1 2))
               (setf (cmark-Node-_string_content heading)
                     (cmark-Node-_string_content container))
               (cmark-Node-insertAfter container heading)
               (cmark-Node-unlink container)
               (setf (cmark-Parser-tip parser) heading)
               (cmark--Parser-advanceOffset
                parser
                (- (length (cmark-Parser-currentLine parser))
                   (cmark-Parser-offset parser))
                nil)
               2))
         0)))

   ;; thematic break
   (lambda (parser _container)
     (if (and (not (cmark-Parser-indented parser))
              (cmark--string-match
               cmark--reThematicBreak
               (substring (cmark-Parser-currentLine parser)
                          (cmark-Parser-nextNonspace parser))))
         (progn
           (cmark--Parser-closeUnmatchedBlocks parser)
           (cmark--Parser-addChild
            parser
            "thematic_break"
            (cmark-Parser-nextNonspace parser))
           (cmark--Parser-advanceOffset
            parser
            (- (length (cmark-Parser-currentLine parser))
               (cmark-Parser-offset parser))
            nil)
           2)
       0))

   ;; list item
   (lambda (parser container)
     (let (data)
       (if (and (or (not (cmark-Parser-indented parser))
                    (equal (cmark-Node-type container) "list"))
                (setq data (cmark--Parser-parseListMarker parser container)))
           (progn
             (cmark--Parser-closeUnmatchedBlocks parser)

             ;; add the list if needed
             (when (or (not (equal (cmark-Node-type
                                    (cmark-Parser-tip parser))
                                   "list"))
                       (not (cmark--listsMatch
                             (cmark-Node-_listData container)
                             data)))
               (setq container (cmark--Parser-addChild
                                parser
                                "list"
                                (cmark-Parser-nextNonspace parser)))
               (setf (cmark-Node-_listData container) data))

             ;; add the list item
             (setq container (cmark--Parser-addChild
                              parser
                              "item"
                              (cmark-Parser-nextNonspace parser)))
             (setf (cmark-Node-_listData container) data)
             1)
         0)))

   ;; indented code block
   (lambda (parser _container)
     (if (and (cmark-Parser-indented parser)
              (not (equal (cmark-Node-type (cmark-Parser-tip parser))
                          "paragraph"))
              (not (cmark-Parser-blank parser)))
         (progn
           ;; indented code
           (cmark--Parser-advanceOffset parser cmark--CODE_INDENT t)
           (cmark--Parser-closeUnmatchedBlocks parser)
           (cmark--Parser-addChild
            parser
            "code_block"
            (cmark-Parser-offset parser))
           2)
       0)))
  "block start functions.  Return values:
0 = no match
1 = matched container, keep going
2 = matched leaf, no more block starts")

(defun cmark--Parser-advanceOffset (this count columns)
  (let ((currentLine (cmark-Parser-currentLine this))
        charsToTab
        charsToAdvance
        c)
    (while (and (> count 0)
                (setq c (cmark--charAt currentLine
                                       (cmark-Parser-offset this))))
      (if (equal c "\t")
          (progn
            (setq charsToTab (- 4 (% (cmark-Parser-column this) 4)))
            (if columns
                (progn
                  (setf (cmark-Parser-partiallyConsumedTab this)
                        (> charsToTab count))
                  (setq charsToAdvance
                        (if (> charsToTab count) count charsToTab))
                  (cl-incf (cmark-Parser-column this) charsToAdvance)
                  (cl-incf (cmark-Parser-offset this)
                           (if (cmark-Parser-partiallyConsumedTab this) 0 1))
                  (setq count (- count charsToAdvance)))
              (setf (cmark-Parser-partiallyConsumedTab this) nil)
              (cl-incf (cmark-Parser-column this) charsToTab)
              (cl-incf (cmark-Parser-offset this) 1)
              (setq count (1- count))))
        (setf (cmark-Parser-partiallyConsumedTab this) nil)
        (cl-incf (cmark-Parser-offset this) 1)
        (cl-incf (cmark-Parser-column this) 1) ;; assume ascii; block starts are ascii
        (setq count (1- count))))))

(defun cmark--Parser-advanceNextNonspace (this)
  (setf (cmark-Parser-offset this) (cmark-Parser-nextNonspace this))
  (setf (cmark-Parser-column this) (cmark-Parser-nextNonspaceColumn this))
  (setf (cmark-Parser-partiallyConsumedTab this) nil))

(defun cmark--Parser-findNextNonspace (this)
  (let ((currentLine (cmark-Parser-currentLine this))
        (i (cmark-Parser-offset this))
        (cols (cmark-Parser-column this))
        c)
    (cl-block while
      (while (not (equal (setq c (cmark--charAt currentLine i)) ""))
        (cond
         ((equal c " ")
          (setq i (1+ i))
          (setq cols (1+ cols)))

         ((equal c "\t")
          (setq i (1+ i))
          (setq cols (+ cols (- 4 (% cols 4)))))

         (t
          (cl-return-from while)))))
    (setf (cmark-Parser-blank this) (or (equal c "\n")
                                         (equal c "\r")
                                         (equal c "")))
    (setf (cmark-Parser-nextNonspace this) i)
    (setf (cmark-Parser-nextNonspaceColumn this) cols)
    (setf (cmark-Parser-indent this) (- (cmark-Parser-nextNonspaceColumn this)
                                         (cmark-Parser-column this)))
    (setf (cmark-Parser-indented this) (>= (cmark-Parser-indent this)
                                            cmark--CODE_INDENT))))

(defun cmark--Parser-incorporateLine (this ln)
  "Analyze a line of text and update the document appropriately.
We parse markdown text by calling this on each line of input,
then finalizing the document."
  (cl-block nil
    (let ((all_matched t)
          type
          (container (cmark-Parser-doc this))
          lastChild
          matchedLeaf
          starts
          startsLen
          i
          lastLineBlank
          cont)
      (setf (cmark-Parser-oldtip this) (cmark-Parser-tip this))
      (setf (cmark-Parser-offset this) 0)
      (setf (cmark-Parser-column this) 0)
      (setf (cmark-Parser-blank this) nil)
      (setf (cmark-Parser-partiallyConsumedTab this) nil)
      (cl-incf (cmark-Parser-lineNumber this) 1)

      ;; replace NUL characters for security
      (when (cl-find ?\u0000 ln)
        (setq ln (cmark--replaceAll "\0" "\uFFFD" ln)))

      (setf (cmark-Parser-currentLine this) ln)

      ;; For each containing block, try to parse the associated line start.
      ;; Bail out on failure: container will point to the last matching block.
      ;; Set all_matched to nil if not all containers match.
      (cl-block while
        (while (and (setq lastChild (cmark-Node-_lastChild container))
                    (cmark-Node-_open lastChild))
          (setq container lastChild)

          (cmark--Parser-findNextNonspace this)

          (cl-case (funcall
                    (cmark--block-continue
                     (gethash (cmark-Node-type container)
                              (cmark-Parser-blocks this)))
                    this
                    container)
            (0 ;; we've matched, keep going
             nil)
            (1 ;; we've failed to match a block
             (setq all_matched nil))
            (2 ;; we've hit end of line for fenced code close and can return
             (setf (cmark-Parser-lastLineLength this) (length ln))
             (cl-return))
            (t
             (error "continue returned illegal value, must be 0, 1, or 2")))
          (when (not all_matched)
            (setq container (cmark-Node-_parent container)) ;; back up to last matching block
            (cl-return-from while))))

      (setf (cmark-Parser-allClosed this)
            (eq container (cmark-Parser-oldtip this)))
      (setf (cmark-Parser-lastMatchedContainer this) container)

      (setq matchedLeaf (and (not (equal (cmark-Node-type container)
                                         "paragraph"))
                             (cmark--block-acceptsLines
                              (gethash (cmark-Node-type container)
                                       cmark--blocks))))
      (setq starts (cmark-Parser-blockStarts this))
      (setq startsLen (length starts))
      ;; Unless last matched container is a code block, try new container starts,
      ;; adding children to the last matched container:
      (cl-block while
        (while (not matchedLeaf)
          (cmark--Parser-findNextNonspace this)

          ;; this is a little performance optimization:
          (when (and (not (cmark-Parser-indented this))
                     (not (cmark--string-match
                           cmark--reMaybeSpecial
                           (substring ln (cmark-Parser-nextNonspace this)))))
            (cmark--Parser-advanceNextNonspace this)
            (cl-return-from while))

          (setq i 0)
          (cl-block while
            (while (< i startsLen)
              (cl-case (funcall (elt starts i) this container)
                (1
                 (setq container (cmark-Parser-tip this))
                 (cl-return-from while))
                (2
                 (setq container (cmark-Parser-tip this))
                 (setq matchedLeaf t)
                 (cl-return-from while))
                (t
                 (setq i (1+ i))))))

          (when (eq i startsLen) ;; nothing matched
            (cmark--Parser-advanceNextNonspace this)
            (cl-return-from while))))

      ;; What remains at the offset is a text line.  Add the text to the
      ;; appropriate container.

      ;; First check for a lazy paragraph continuation:
      (if (and (not (cmark-Parser-allClosed this))
               (not (cmark-Parser-blank this))
               (equal (cmark-Node-type (cmark-Parser-tip this)) "paragraph"))
          ;; lazy paragraph continuation
          (cmark--Parser-addLine this)

        ;; not a lazy continuation

        ;; finalize any blocks not matched
        (cmark--Parser-closeUnmatchedBlocks this)
        (when (and (cmark-Parser-blank this)
                   (cmark-Node-lastChild container))
          (setf (cmark-Node-_lastLineBlank (cmark-Node-lastChild container))
                t))

        (setq type (cmark-Node-type container))

        ;; Block quote lines are never blank as they start with >
        ;; and we don't count blanks in fenced code for purposes of tight/loose
        ;; lists or breaking out of lists.  We also don't set _lastLineBlank
        ;; on an empty list item, or if we just closed a fenced block.
        (setq lastLineBlank
              (and (cmark-Parser-blank this)
                   (not (or (equal type "block_quote")
                            (and (equal type "code_block")
                                 (cmark-Node-_isFenced container))
                            (and (equal type "item")
                                 (not (cmark-Node-_firstChild container))
                                 (eq (car (car
                                           (cmark-Node-sourcepos container)))
                                     (cmark-Parser-lineNumber this)))))))

        ;; propagate lastLineBlank up through parents:
        (setq cont container)
        (while cont
          (setf (cmark-Node-_lastLineBlank cont) lastLineBlank)
          (setq cont (cmark-Node-_parent cont)))

        (cond
         ((cmark--block-acceptsLines (gethash type (cmark-Parser-blocks this)))
          (cmark--Parser-addLine this)
          ;; if HtmlBlock, check for end condition
          (when (and (equal type "html_block")
                     (>= (cmark-Node-_htmlBlockType container) 1)
                     (<= (cmark-Node-_htmlBlockType container) 5)
                     (cmark--string-match
                      (elt cmark--reHtmlBlockClose
                           (cmark-Node-_htmlBlockType container))
                      (substring (cmark-Parser-currentLine this)
                                 (cmark-Parser-offset this))))
            (cmark--Parser-finalize this
                                    container
                                    (cmark-Parser-lineNumber this))))
         ((and (< (cmark-Parser-offset this) (length ln))
               (and (not (cmark-Parser-blank this))))
          ;; create paragraph container for line
          (setq container (cmark--Parser-addChild
                           this
                           "paragraph"
                           (cmark-Parser-offset this)))
          (cmark--Parser-advanceNextNonspace this)
          (cmark--Parser-addLine this))))
      (setf (cmark-Parser-lastLineLength this) (length ln)))))

(defun cmark--Parser-finalize (this block lineNumber)
  "Finalize a block.  Close it and do any necessary postprocessing,
e.g. creating string_content from strings, setting the \"tight\"
or \"loose\" status of a list, and parsing the beginnings
of paragraphs for reference definitions.  Reset the tip to the
parent of the closed block."
  (let ((above (cmark-Node-_parent block)))
    (setf (cmark-Node-_open block) nil)
    (setcdr (cmark-Node-sourcepos block)
            (cons lineNumber
                  (cmark-Parser-lastLineLength this)))

    (funcall
     (cmark--block-finalize (gethash (cmark-Node-type block)
                                     (cmark-Parser-blocks this)))
     this
     block)

    (setf (cmark-Parser-tip this) above)))

(defun cmark--Parser-processInlines (this block)
  "Walk through a block & children recursively, parsing string content
into inline content where appropriate."
  (let (node
        event
        type
        (walker (cmark-Node-walker block)))
    (setf (cmark--InlineParser-refmap (cmark-Parser-inlineParser this))
          (cmark-Parser-refmap this))
    (setf (cmark--InlineParser-options (cmark-Parser-inlineParser this))
          (cmark-Parser-options this))
    (while (setq event (cmark-NodeWalker-next walker))
      (setq node (cmark-event-node event))
      (setq type (cmark-Node-type node))
      (when (and (not (cmark-event-entering event))
                 (or (equal type "paragraph") (equal type "heading")))
        (cmark--InlineParser-parse (cmark-Parser-inlineParser this) node)))))

(defun cmark--create-Document ()
  (cmark-create-Node "document" (cons (cons 1 1) (cons 0 0))))

(defun cmark-Parser-parse (this input)
  "The main parsing function.  Returns a parsed document AST."
  (setf (cmark-Parser-doc this) (cmark--create-Document))
  (setf (cmark-Parser-tip this) (cmark-Parser-doc this))
  (setf (cmark-Parser-refmap this) (make-hash-table :test 'equal))
  (setf (cmark-Parser-lineNumber this) 0)
  (setf (cmark-Parser-lastLineLength this) 0)
  (setf (cmark-Parser-offset this) 0)
  (setf (cmark-Parser-column this) 0)
  (setf (cmark-Parser-lastMatchedContainer this) (cmark-Parser-doc this))
  (setf (cmark-Parser-currentLine this) "")

  (let (start-time-preparing-input
        start-time-block-parsing
        start-time-inline-parsing
        lines
        len
        i)

    (when (cmark-options-time (cmark-Parser-options this))
      (setq start-time-preparing-input (current-time)))
    (setq lines (split-string input cmark--reLineEnding))
    (setq len (length lines))

    (when (eq (elt input (1- (length input))) cmark--C_NEWLINE)
      ;; ignore last blank line created by final newline
      (setq len (1- len)))
    (when (cmark-options-time (cmark-Parser-options this))
      (message "preparing input: %f"
               (time-to-seconds (time-since start-time-preparing-input))))

    (when (cmark-options-time (cmark-Parser-options this))
      (setq start-time-block-parsing (current-time)))

    (setq i 0)
    (while (< i len)
      (cmark--Parser-incorporateLine this (nth i lines))
      (setq i (1+ i)))

    (while (cmark-Parser-tip this)
      (cmark--Parser-finalize this (cmark-Parser-tip this) len))

    (when (cmark-options-time (cmark-Parser-options this))
      (message "block parsing: %f"
               (time-to-seconds (time-since start-time-block-parsing))))

    (when (cmark-options-time (cmark-Parser-options this))
      (setq start-time-inline-parsing (current-time)))
    (cmark--Parser-processInlines this (cmark-Parser-doc this))
    (when (cmark-options-time (cmark-Parser-options this))
      (message "inline parsing: %f"
               (time-to-seconds (time-since start-time-inline-parsing))))

    (cmark-Parser-doc this)))

  ;; The Parser object.
(defun cmark-create-Parser (&optional options)
  (let ((doc (cmark--create-Document)))
    (make-cmark-Parser
     :doc doc
     :blocks cmark--blocks
     :blockStarts cmark--blockStarts
     :tip doc
     :oldtip doc
     :currentLine ""
     :lineNumber 0
     :offset 0
     :column 0
     :nextNonspace 0
     :nextNonspaceColumn 0
     :indent 0
     :indented nil
     :blank nil
     :partiallyConsumedTab nil
     :allClosed t
     :lastMatchedContainer doc
     :refmap (make-hash-table :test 'equal)
     :lastLineLength 0
     :inlineParser (cmark--create-InlineParser options)
     :options (or options (make-cmark-options)))))

(provide 'cmark-blocks)

;;; cmark-blocks.el ends here
