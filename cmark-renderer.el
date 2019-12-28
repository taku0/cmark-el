;;; cmark-renderer.el --- Base class for renderers. -*- lexical-binding: t -*-

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

;; Base class for renderers.

;;; Code

(cl-defstruct cmark-Renderer
  buffer
  lastOut
  handlers)

(defun cmark-Renderer-render (this ast)
  "Walks the AST and calls member methods for each Node type.

AST: The root of the abstract syntax tree."
  (let ((walker (cmark-Node-walker ast))
        event
        type)
    (setf (cmark-Renderer-buffer this) "")
    (setf (cmark-Renderer-lastOut this) "\n")

    (while (setq event (cmark-NodeWalker-next walker))
      (setq type (cmark-Node-type (cmark-event-node event)))
      (when (gethash type (cmark-Renderer-handlers this))
        (funcall (gethash type (cmark-Renderer-handlers this))
                 this
                 (cmark-event-node event)
                 (cmark-event-entering event))))
    (cmark-Renderer-buffer this)))

(defun cmark--Renderer-lit (this str)
  "Concatenate a literal string to the buffer.

STR: The string to concatenate."
  (cl-callf concat (cmark-Renderer-buffer this) str)
  (setf (cmark-Renderer-lastOut this) str))

(defun cmark--Renderer-cr (this)
  "Output a newline to the buffer."
  (when (not (equal (cmark-Renderer-lastOut this) "\n"))
    (cmark--Renderer-lit this "\n")))

(defun cmark--Renderer-out (this str)
  "Concatenate a string to the buffer possibly escaping the content.

Concrete renderer implementations should override this method.

STR: The string to concatenate."
  (cmark--Renderer-lit this str))

(defun cmark--Renderer-esc (_this str)
  "Escape a string for the target renderer.

Abstract function that should be implemented by concrete
renderer implementations.

STR: The string to escape."
  str)

(provide 'cmark-renderer)

;;; cmark-renderer.el ends here
