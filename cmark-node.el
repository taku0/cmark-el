;;; cmark-node.el --- CommonMark document tree node and walker. -*- lexical-binding: t -*-

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

;; Document tree node and walker.
;;
;; Example of use of walker:
;;
;; (let ((walker (cmark--Node-walker root))
;;       event)
;;   (while (setq event (cmark--NodeWalker-next walker))
;;     (prin1 (cmark-event-entering event))
;;     (prin1 (cmark--Node-type (cmark-event-node event)))))

;;; Code

(require 'cl-macs)

(defun cmark--Node-isContainer (node)
  (member
   (cmark-Node-_type node)
   '("document"
     "block_quote"
     "list"
     "item"
     "paragraph"
     "heading"
     "emph"
     "strong"
     "link"
     "image"
     "custom_inline"
     "custom_block")))

(cl-defstruct cmark-event
  entering
  node)

(cl-defstruct cmark-NodeWalker
  current
  root
  entering)

(defun cmark-create-NodeWalker (root)
  (make-cmark-NodeWalker
   :current root
   :root root
   :entering t))

(defun cmark--NodeWalker-resumeAt (this node entering)
  (setf (cmark-NodeWalker-current this) node)
  (setf (cmark-NodeWalker-entering this) entering))

(defun cmark-NodeWalker-next (this)
  (let ((cur (cmark-NodeWalker-current this))
        (entering (cmark-NodeWalker-entering this))
        container)
    (if (null cur)
        nil
      (setq container (cmark--Node-isContainer cur))
      (cond
       ((and entering container)
        (if (cmark-Node-_firstChild cur)
            (progn
              (setf (cmark-NodeWalker-current this)
                    (cmark-Node-_firstChild cur))
              (setf (cmark-NodeWalker-entering this) t))
          ;; stay on node but exit
          (setf (cmark-NodeWalker-entering this) nil)))
       ((equal cur (cmark-NodeWalker-root this))
        (setf (cmark-NodeWalker-current this) nil))
       ((null (cmark-Node-_next cur))
        (setf (cmark-NodeWalker-current this) (cmark-Node-_parent cur))
        (setf (cmark-NodeWalker-entering this) nil))
       (t
        (setf (cmark-NodeWalker-current this) (cmark-Node-_next cur))
        (setf (cmark-NodeWalker-entering this) t)))
      (make-cmark-event
       :entering entering
       :node cur))))

(cl-defstruct cmark-Node
  _type
  _parent
  _firstChild
  _lastChild
  _prev
  _next
  _sourcepos
  _lastLineBlank
  _lastLineChecked
  _open
  _string_content
  _literal
  _listData
  _info
  _destination
  _title
  _isFenced
  _fenceChar
  _fenceLength
  _fenceOffset
  _level
  _onEnter
  _onExit
  _htmlBlockType)

(defun cmark-create-Node (nodeType &optional sourcepos)
  (make-cmark-Node
   :type nodeType
   :parent nil
   :firstChild nil
   :lastChild nil
   :prev nil
   :next nil
   :sourcepos sourcepos
   :lastLineBlank nil
   :lastLineChecked nil
   :open t
   :string_content nil
   :literal nil
   :listData (make-cmark--_listData)
   :info nil
   :destination nil
   :title nil
   :isFenced nil
   :fenceChar nil
   :fenceLength 0
   :fenceOffset nil
   :level nil
   :onEnter nil
   :onExit nil))

(defun cmark--Node-show (this)
  (let ((children '())
        (child (cmark-Node-_firstChild this)))
    (while child
      (push child children)
      (setq child (cmark-Node-_next child)))

    (concat
     "("
     (cmark-Node-_type this)
     " "
     (mapconcat #'cmark--Node-show (nreverse children) " ")
     (if (cmark-Node-literal this) (concat " " (cmark-Node-literal this)) "")
     ")")))

(cl-defstruct cmark--_listData
  type
  tight
  start
  delimiter)

(defalias 'cmark-Node-type 'cmark-Node-_type)
(defalias 'cmark-Node-firstChild 'cmark-Node-_firstChild)
(defalias 'cmark-Node-lastChild 'cmark-Node-_lastChild)
(defalias 'cmark-Node-next 'cmark-Node-_next)
(defalias 'cmark-Node-prev 'cmark-Node-_prev)
(defalias 'cmark-Node-parent 'cmark-Node-_parent)
(defalias 'cmark-Node-sourcepos 'cmark-Node-_sourcepos)
(defalias 'cmark-Node-literal 'cmark-Node-_literal)
(defalias 'cmark-Node-destination 'cmark-Node-_destination)
(defalias 'cmark-Node-title 'cmark-Node-_title)
(defalias 'cmark-Node-info 'cmark-Node-_info)
(defalias 'cmark-Node-level 'cmark-Node-_level)
(defalias 'cmark-Node-onEnter 'cmark-Node-_onEnter)
(defalias 'cmark-Node-onExit 'cmark-Node-_onExit)

(defun cmark-Node-listType (this)
  (cmark--listMarker-type (cmark-Node-_listData this)))
(defun cmark-Node-listTight (this)
  (cmark--listMarker-tight (cmark-Node-_listData this)))
(defun cmark-Node-listStart (this)
  (cmark--listMarker-start (cmark-Node-_listData this)))
(defun cmark-Node-listDelimiter (this)
  (cmark--listMarker-delimiter (cmark-Node-_listData this)))

(defun cmark-Node-appendChild (this child)
  (cmark-Node-unlink child)
  (setf (cmark-Node-_parent child) this)
  (if (cmark-Node-_lastChild this)
      (progn
        (setf (cmark-Node-_next (cmark-Node-_lastChild this)) child)
        (setf (cmark-Node-_prev child) (cmark-Node-_lastChild this))
        (setf (cmark-Node-_lastChild this) child))
    (setf (cmark-Node-_firstChild this) child)
    (setf (cmark-Node-_lastChild this) child)))

(defun cmark-Node-prependChild (this child)
  (cmark-Node-unlink child)
  (setf (cmark-Node-_parent child) this)
  (if (cmark-Node-_firstChild this)
      (progn
        (setf (cmark-Node-_prev (cmark-Node-_firstChild this)) child)
        (setf (cmark-Node-_next child) (cmark-Node-_firstChild this))
        (setf (cmark-Node-_firstChild this) child))
    (setf (cmark-Node-_firstChild this) child)
    (setf (cmark-Node-_lastChild this) child)))

(defun cmark-Node-unlink (this)
  (if (cmark-Node-_prev this)
      (setf (cmark-Node-_next (cmark-Node-_prev this))
            (cmark-Node-_next this))
    (when (cmark-Node-_parent this)
      (setf (cmark-Node-_firstChild (cmark-Node-_parent this))
            (cmark-Node-_next this))))

  (if (cmark-Node-_next this)
      (setf (cmark-Node-_prev (cmark-Node-_next this))
            (cmark-Node-_prev this))
    (when (cmark-Node-_parent this)
      (setf (cmark-Node-_lastChild (cmark-Node-_parent this))
            (cmark-Node-_prev this))))
  (setf (cmark-Node-_parent this) nil)
  (setf (cmark-Node-_next this) nil)
  (setf (cmark-Node-_prev this) nil))

(defun cmark-Node-insertAfter (this sibling)
  (cmark-Node-unlink sibling)
  (setf (cmark-Node-_next sibling) (cmark-Node-_next this))
  (when (cmark-Node-_next sibling)
    (setf (cmark-Node-_prev (cmark-Node-_next sibling)) sibling))
  (setf (cmark-Node-_prev sibling) this)
  (setf (cmark-Node-_next this) sibling)
  (setf (cmark-Node-_parent sibling) (cmark-Node-_parent this))
  (when (null (cmark-Node-_next sibling))
    (setf (cmark-Node-_lastChild (cmark-Node-_parent sibling)) sibling)))

(defun cmark-Node-insertBefore (this sibling)
  (cmark-Node-unlink sibling)
  (setf (cmark-Node-_prev sibling) (cmark-Node-_prev this))
  (when (cmark-Node-_prev sibling)
    (setf (cmark-Node-_next (cmark-Node-_prev sibling)) sibling))
  (setf (cmark-Node-_next sibling) this)
  (setf (cmark-Node-_prev this) sibling)
  (setf (cmark-Node-_parent sibling) (cmark-Node-_parent this))
  (when (null (cmark-Node-_prev sibling))
    (setf (cmark-Node-_firstChild (cmark-Node-_parent sibling)) sibling)))

(defalias 'cmark-Node-walker 'cmark-create-NodeWalker)

(provide 'cmark-node)

;;; cmark-node.el ends here
