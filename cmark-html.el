;;; cmark-html.el --- HTML renderer. -*- lexical-binding: t -*-

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

;; HTML renderer.
;;
;; Usage:
;;
;; (let* ((renderer (cmark-create-HtmlRenderer)))
;;   (princ (cmark-HtmlRenderer-render root)))

;;; Code

(require 'cmark-renderer)

(defconst cmark--reUnsafeProtocol
  (list "\\`javascript:\\|vbscript:\\|file:\\|data:" :ignore-case))
(defconst cmark--reSafeDataProtocol
  (list "\\`data:image/\\(?:png\\|gif\\|jpeg\\|webp\\)" :ignore-case))

(defun cmark--potentiallyUnsafe (url)
  (and (cmark--string-match cmark--reUnsafeProtocol url)
       (cmark--string-match cmark--reSafeDataProtocol url)))

(cl-defstruct cmark-HtmlRenderer-options
  softbreak
  safe
  sourcepos)

(defconst cmark--HtmlRenderer-handlers
  (let ((handlers (make-hash-table :test 'equal)))
    (puthash "text" #'cmark--HtmlRenderer-text handlers)
    (puthash "html_inline" #'cmark--HtmlRenderer-html_inline handlers)
    (puthash "html_block" #'cmark--HtmlRenderer-html_block handlers)
    (puthash "softbreak" #'cmark--HtmlRenderer-softbreak handlers)
    (puthash "linebreak" #'cmark--HtmlRenderer-linebreak handlers)
    (puthash "link" #'cmark--HtmlRenderer-link handlers)
    (puthash "image" #'cmark--HtmlRenderer-image handlers)
    (puthash "emph" #'cmark--HtmlRenderer-emph handlers)
    (puthash "strong" #'cmark--HtmlRenderer-strong handlers)
    (puthash "paragraph" #'cmark--HtmlRenderer-paragraph handlers)
    (puthash "heading" #'cmark--HtmlRenderer-heading handlers)
    (puthash "code" #'cmark--HtmlRenderer-code handlers)
    (puthash "code_block" #'cmark--HtmlRenderer-code_block handlers)
    (puthash "thematic_break" #'cmark--HtmlRenderer-thematic_break handlers)
    (puthash "block_quote" #'cmark--HtmlRenderer-block_quote handlers)
    (puthash "list" #'cmark--HtmlRenderer-list handlers)
    (puthash "item" #'cmark--HtmlRenderer-item handlers)
    (puthash "custom_inline" #'cmark--HtmlRenderer-custom_inline handlers)
    (puthash "custom_block" #'cmark--HtmlRenderer-custom_block handlers)
    handlers))

(cl-defstruct (cmark-HtmlRenderer (:include cmark--Renderer))
  disableTags
  options)

(defun cmark-create-HtmlRenderer (&optional options)
  (let ((this (make-cmark-HtmlRenderer)))
    (setq options (or options (make-cmark-HtmlRenderer-options)))
    ;; by default, soft breaks are rendered as newlines in HTML
    (cl-callf or (cmark-HtmlRenderer-options-softbreak options) "\n")
    ;; set to "<br />" to make them hard breaks
    ;; set to " " if you want to ignore line wrapping in source

    (setf (cmark-HtmlRenderer-disableTags this) 0)
    (setf (cmark-HtmlRenderer-lastOut this) "\n")
    (setf (cmark-HtmlRenderer-options this) options)

    (setf (cmark-HtmlRenderer-handlers this) cmark--HtmlRenderer-handlers)
    this))

;; Helper function to produce an HTML tag.
(defun cmark--HtmlRenderer-tag (this name &optional attrs selfclosing)
  (cl-block nil
    (when (> (cmark-HtmlRenderer-disableTags this) 0)
      (cl-return))
    (cl-callf concat (cmark-HtmlRenderer-buffer this) (concat "<" name))
    (when (and attrs (> (length attrs) 0))
      (mapc (lambda (attrib)
              (cl-callf concat (cmark-HtmlRenderer-buffer this)
                (concat " " (car attrib) "=\"" (cdr attrib) "\"")))
            attrs))
    (when selfclosing
      (cl-callf concat (cmark-HtmlRenderer-buffer this) " /"))
    (cl-callf concat (cmark-HtmlRenderer-buffer this) ">")
    (setf (cmark-HtmlRenderer-lastOut this) ">")))

;;; Node methods

(defun cmark--HtmlRenderer-text (this node &optional _entering)
  (cmark--HtmlRenderer-out this (cmark-Node-literal node)))

(defun cmark--HtmlRenderer-softbreak (this &optional _node _entering)
  (cmark--HtmlRenderer-lit
   this
   (cmark-HtmlRenderer-options-softbreak (cmark-HtmlRenderer-options this))))

(defun cmark--HtmlRenderer-linebreak (this &optional _node _entering)
  (cmark--HtmlRenderer-tag this "br" [] t)
  (cmark--HtmlRenderer-cr this))

(defun cmark--HtmlRenderer-link (this node entering)
  (let ((attrs (cmark--HtmlRenderer-attrs this node)))
    (if entering
        (progn
          (when (not
                 (and (cmark-HtmlRenderer-options-safe
                       (cmark-HtmlRenderer-options this))
                      (cmark--potentiallyUnsafe
                       (cmark-Node-destination node))))
            (push (cons "href"
                        (cmark--HtmlRenderer-esc
                         this
                         (cmark-Node-destination node)
                         nil))
                  attrs))
          (when (and (cmark-Node-title node)
                     (not (equal (cmark-Node-title node) "")))
            (push (cons "title"
                        (cmark--HtmlRenderer-esc
                         this
                         (cmark-Node-title node)
                         nil))
                  attrs))
          (cmark--HtmlRenderer-tag this "a" (reverse attrs)))
      (cmark--HtmlRenderer-tag this "/a"))))

(defun cmark--HtmlRenderer-image (this node entering)
  (if entering
      (progn
        (when (zerop (cmark-HtmlRenderer-disableTags this))
          (if (and (cmark-HtmlRenderer-options-safe
                    (cmark-HtmlRenderer-options this))
                   (cmark--potentiallyUnsafe (cmark-Node-destination node)))
              (cmark--HtmlRenderer-lit this "<img src=\"\" alt=\"")
            (cmark--HtmlRenderer-lit
             this
             (concat
              "<img src=\""
              (cmark--HtmlRenderer-esc this (cmark-Node-destination node) nil)
              "\" alt=\""))))
        (cl-incf (cmark-HtmlRenderer-disableTags this)))
    (cl-decf (cmark-HtmlRenderer-disableTags this))
    (when (zerop (cmark-HtmlRenderer-disableTags this))
      (when (and (cmark-Node-title node)
                 (not (equal (cmark-Node-title node) "")))
        (cmark--HtmlRenderer-lit
         this
         (concat
          "\" title=\""
          (cmark--HtmlRenderer-esc this (cmark-Node-title node) nil))))
      (cmark--HtmlRenderer-lit this "\" />"))))

(defun cmark--HtmlRenderer-emph (this node entering)
  (cmark--HtmlRenderer-tag this (if entering "em" "/em")))

(defun cmark--HtmlRenderer-strong (this node entering)
  (cmark--HtmlRenderer-tag this (if entering "strong" "/strong")))

(defun cmark--HtmlRenderer-paragraph (this node entering)
  (cl-block nil
    (let ((grandparent (cmark-Node-parent (cmark-Node-parent node)))
          (attrs (cmark--HtmlRenderer-attrs this node)))
      (when (and (not (null grandparent))
                 (equal (cmark-Node-type grandparent) "list"))
        (when (cmark-Node-listTight grandparent)
          (cl-return)))
      (if entering
          (progn
            (cmark--HtmlRenderer-cr this)
            (cmark--HtmlRenderer-tag this "p" attrs))
        (cmark--HtmlRenderer-tag this "/p")
        (cmark--HtmlRenderer-cr this)))))

(defun cmark--HtmlRenderer-heading (this node entering)
  (let ((tagname (concat "h" (number-to-string (cmark-Node-level node))))
        (attrs (cmark--HtmlRenderer-attrs this node)))
    (if entering
        (progn
          (cmark--HtmlRenderer-cr this)
          (cmark--HtmlRenderer-tag this tagname attrs))
      (cmark--HtmlRenderer-tag this (concat "/" tagname))
      (cmark--HtmlRenderer-cr this))))

(defun cmark--HtmlRenderer-code (this node &optional _entering)
  (cmark--HtmlRenderer-tag this "code")
  (cmark--HtmlRenderer-out this (cmark-Node-literal node))
  (cmark--HtmlRenderer-tag this "/code"))

(defun cmark--HtmlRenderer-code_block (this node &optional _entering)
  (let ((info_words (if (cmark-Node-info node)
                        (split-string
                         (cmark-Node-info node)
                         (concat cmark--SPACE "+"))
                      '()))
        (attrs (cmark--HtmlRenderer-attrs this node)))
    (when (and (> (length info_words) 0)
               (> (length (car info_words)) 0))
      (push (cons "class"
                  (concat "language-"
                          (cmark--HtmlRenderer-esc this (car info_words) nil)))
            attrs))
    (cmark--HtmlRenderer-cr this)
    (cmark--HtmlRenderer-tag this "pre")
    (cmark--HtmlRenderer-tag this "code" (reverse attrs))
    (cmark--HtmlRenderer-out this (cmark-Node-literal node))
    (cmark--HtmlRenderer-tag this "/code")
    (cmark--HtmlRenderer-tag this "/pre")
    (cmark--HtmlRenderer-cr this)))

(defun cmark--HtmlRenderer-thematic_break (this node &optional _entering)
  (let ((attrs (cmark--HtmlRenderer-attrs this node)))
    (cmark--HtmlRenderer-cr this)
    (cmark--HtmlRenderer-tag this "hr" attrs t)
    (cmark--HtmlRenderer-cr this)))

(defun cmark--HtmlRenderer-block_quote (this node entering)
  (let ((attrs (cmark--HtmlRenderer-attrs this node)))
    (if entering
        (progn
          (cmark--HtmlRenderer-cr this)
          (cmark--HtmlRenderer-tag this "blockquote" attrs)
          (cmark--HtmlRenderer-cr this))
      (cmark--HtmlRenderer-cr this)
      (cmark--HtmlRenderer-tag this "/blockquote")
      (cmark--HtmlRenderer-cr this))))

(defun cmark--HtmlRenderer-list (this node entering)
  (let ((tagname (if (equal (cmark-Node-listType node) "bullet") "ul" "ol"))
        (attrs (cmark--HtmlRenderer-attrs this node))
        start)
    (if entering
        (progn
          (setq start (cmark-Node-listStart node))
          (when (and (not (null start))
                     (not (eq start 1)))
            (push (cons "start" (number-to-string start)) attrs))
          (cmark--HtmlRenderer-cr this)
          (cmark--HtmlRenderer-tag this tagname (reverse attrs))
          (cmark--HtmlRenderer-cr this))
      (cmark--HtmlRenderer-cr this)
      (cmark--HtmlRenderer-tag this (concat "/" tagname))
      (cmark--HtmlRenderer-cr this))))

(defun cmark--HtmlRenderer-item (this node entering)
  (let ((attrs (cmark--HtmlRenderer-attrs this node)))
    (if entering
        (cmark--HtmlRenderer-tag this "li" attrs)
      (cmark--HtmlRenderer-tag this "/li")
      (cmark--HtmlRenderer-cr this))))

(defun cmark--HtmlRenderer-html_inline (this node &optional _entering)
  (if (cmark-HtmlRenderer-options-safe (cmark-HtmlRenderer-options this))
      (cmark--HtmlRenderer-lit this "<!-- raw HTML omitted -->")
    (cmark--HtmlRenderer-lit this (cmark-Node-literal node))))

(defun cmark--HtmlRenderer-html_block (this node &optional _entering)
  (cmark--HtmlRenderer-cr this)
  (if (cmark-HtmlRenderer-options-safe (cmark-HtmlRenderer-options this))
      (cmark--HtmlRenderer-lit this "<!-- raw HTML omitted -->")
    (cmark--HtmlRenderer-lit this (cmark-Node-literal node)))
  (cmark--HtmlRenderer-cr this))

(defun cmark--HtmlRenderer-custom_inline (this node entering)
  (cond
   ((and entering (cmark-Node-onEnter node))
    (cmark--HtmlRenderer-lit this (cmark-Node-onEnter node)))

   ((and (not entering) (cmark-Node-onExit node))
    (cmark--HtmlRenderer-lit this (cmark-Node-onExit node)))))

(defun cmark--HtmlRenderer-custom_block (this node entering)
  (cmark--HtmlRenderer-cr this)
  (cond
   ((and entering (cmark-Node-onEnter node))
    (cmark--HtmlRenderer-lit this (cmark-Node-onEnter node)))

   ((and (not entering) (cmark-Node-onExit node))
    (cmark--HtmlRenderer-lit this (cmark-Node-onExit node))))
  (cmark--HtmlRenderer-cr this))

;;; Helper methods

(defun cmark--HtmlRenderer-out (this s)
  (cmark--HtmlRenderer-lit this (cmark--HtmlRenderer-esc this s nil)))

(defun cmark--HtmlRenderer-attrs (this node)
  (let ((att '())
        pos)
    (when (cmark-HtmlRenderer-options-sourcepos
           (cmark-HtmlRenderer-options this))
      (setq pos (cmark-Node-sourcepos node))
      (when pos
        (push (cons "data-sourcepos"
                    (concat
                     (number-to-string (car (car pos)))
                     ":"
                     (number-to-string (cdr (car pos)))
                     "-"
                     (number-to-string (car (cdr pos)))
                     ":"
                     (number-to-string (cdr (cdr pos)))))
              att)))
    att))

(defalias 'cmark-HtmlRenderer-render 'cmark--Renderer-render)
(defalias 'cmark--HtmlRenderer-lit 'cmark--Renderer-lit)
(defalias 'cmark--HtmlRenderer-cr 'cmark--Renderer-cr)

(defun cmark--HtmlRenderer-esc (_this s _preserve_entities)
  (cmark--escapeXml s))

(provide 'cmark-html)

;;; cmark-html.el ends her
