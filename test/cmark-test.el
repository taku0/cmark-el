;;; cmark-test.el --- Tests for cmark -*- lexical-binding: t -*-

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

;; This file provides tests for the parser and the HTML renderer:
;;
;; - Examples from CommonMark spec
;; - Tests for smart punctuations
;; - Regression tests
;; - Pathological cases
;;
;; Usage:
;;
;; (cmark-run-test-cases)

;;; Code

(require 'cmark)
(require 'cmark-html)

(defvar cmark--test-basedir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun cmark--setup-error-buffer ()
  "Initialize and switch to the error buffer.

Return the error-buffer"
  (pop-to-buffer (get-buffer-create "*cmark-test*"))
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (current-buffer))

(defun cmark--print-message (error-buffer message)
  "Print a message to the ERROR-BUFFER or stdout.

If the Emacs is in the batch mode, MESSAGE is printed to the stdout.
Otherwise, MESSAGE is appended to the ERROR-BUFFER."
  (if noninteractive
      (princ message)
    (with-current-buffer error-buffer
      (goto-char (point-max))
      (insert-and-inherit message))))

(defvar-local cmark--test-color-start-marker nil)
(defvar-local cmark--test-color nil)

 ;; Home made mini-version of the npm ansi module:
(defun cmark--escSeq (s)
  (princ "\u001b")
  (princ s))

(defalias 'cmark--cursor-write 'cmark--print-message)

(defun cmark--cursor-color (error-buffer color-sequence color)
  (if noninteractive
      (cmark--escSeq color-sequence)
    (with-current-buffer error-buffer
      (when (marker-position cmark--test-color-start-marker)
        (add-face-text-property
         cmark--test-color-start-marker
         (point)
         (list :foreground cmark--test-color)))
      (setq-local cmark--test-color color)
      (move-marker cmark--test-color-start-marker (point)))))

(defun cmark--cursor-green (error-buffer)
  (cmark--cursor-color error-buffer "[0;32m" "green"))

(defun cmark--cursor-red (error-buffer)
  (cmark--cursor-color error-buffer "[0;31m" "red"))

(defun cmark--cursor-cyan (error-buffer)
  (cmark--cursor-color error-buffer "[0;36m" "cyan"))

(defun cmark--cursor-reset (error-buffer)
  (if noninteractive
      (cmark--escSeq "[0m")
    (when (marker-position cmark--test-color-start-marker)
      (with-current-buffer error-buffer
        (add-face-text-property
         cmark--test-color-start-marker
         (point)
         (list :foreground cmark--test-color))
        (move-marker cmark--test-color-start-marker nil)))))

(defun cmark--repeat (pattern count)
  (if (< count 1)
      ""
    (let ((result ""))
      (while (> count 1)
        (when (= (logand count 1) 1)
          (setq result (concat result pattern)))
        (setq count (lsh count -1))
        (setq pattern (concat pattern pattern)))
      (concat result pattern))))

(defun cmark--showSpaces (s)
  (cmark--replaceAll " " "␣" (cmark--replaceAll "\t" "→" s)))

(cl-defstruct cmark--example
  markdown
  html
  section
  number)

(defun cmark--extractSpecTests (testfile)
  (with-temp-buffer
    (insert-file-contents testfile)
    (let ((examples '())
          (current_section "")
          (example_number 0)
          beginning-of-example
          beginning-of-html
          markdownSubmatch
          htmlSubmatch)
      (when (search-forward-regexp "^<!-- END TESTS -->" nil t)
        (forward-line 0)
        (delete-region (point) (point-max)))
      (goto-char (point-min))
      (while (search-forward-regexp
              "^`\\{32\\} example$\\|^#\\{1,6\\} *\\(.*\\)$"
              nil
              t)
        (if (match-string 1)
            (setq current_section (match-string 1))
          (setq beginning-of-example (1+ (match-end 0)))
          (setq example_number (1+ example_number))
          (search-forward-regexp "^\\.$" nil t)
          (setq beginning-of-html (1+ (match-end 0)))
          (setq markdownSubmatch
                (buffer-substring-no-properties
                 beginning-of-example
                 (match-beginning 0)))
          (search-forward-regexp "^`\\{32\\}$" nil t)
          (setq htmlSubmatch
                (buffer-substring-no-properties
                 beginning-of-html
                 (match-beginning 0)))
          (push
           (make-cmark--example
            :markdown markdownSubmatch
            :html htmlSubmatch
            :section current_section
            :number example_number)
           examples)))
      (nreverse examples))))

(defun cmark--specTest (error-buffer testcase res converter)
  (let* ((markdown (cmark--replaceAll "→"
                                      "\t"
                                      (cmark--example-markdown testcase)))
         (expected (cmark--replaceAll "→"
                                      "\t"
                                      (cmark--example-html testcase)))
         (actual (funcall converter markdown)))
    (if (equal actual expected)
        (progn
          (setcar res (1+ (car res)))
          (cmark--cursor-green error-buffer)
          (cmark--cursor-write error-buffer "✓")
          (cmark--cursor-reset error-buffer))
      (setcdr res (1+ (cdr res)))
      (cmark--cursor-write error-buffer "\n")
      (cmark--cursor-red error-buffer)
      (cmark--cursor-write error-buffer
                           (concat "✘ Example "
                                   (number-to-string
                                    (cmark--example-number testcase))
                                   "\n"))
      (cmark--cursor-cyan error-buffer)
      (cmark--cursor-write error-buffer "=== markdown ===============\n")
      (cmark--cursor-write error-buffer (cmark--showSpaces markdown))
      (cmark--cursor-write error-buffer "=== expected ===============\n")
      (cmark--cursor-write error-buffer (cmark--showSpaces expected))
      (cmark--cursor-write error-buffer "=== got ====================\n")
      (cmark--cursor-write error-buffer (cmark--showSpaces actual))
      (cmark--cursor-reset error-buffer))))

(defun cmark--specTests (error-buffer testfile res converter)
  (cmark--cursor-write error-buffer (concat "Spec tests [" testfile "]:\n"))

  (let ((current_section "")
        (examples (cmark--extractSpecTests testfile))
        (start-time (current-time)))

    (dolist (testcase examples)
      (unless (equal (cmark--example-section testcase) current_section)
        (unless (equal current_section "")
          (cmark--cursor-write error-buffer "\n"))
        (setq current_section (cmark--example-section testcase))
        (cmark--cursor-reset error-buffer)
        (cmark--cursor-write error-buffer current_section)
        (cmark--cursor-reset error-buffer)
        (cmark--cursor-write error-buffer "  "))
      (cmark--specTest error-buffer testcase res converter))
    (cmark--cursor-write error-buffer "\n")
    (cmark--cursor-write error-buffer "Elapsed time ")
    (cmark--cursor-write error-buffer
                         (number-to-string
                          (time-to-seconds
                           (time-since start-time))))
    (cmark--cursor-write error-buffer "\n")
    (cmark--cursor-write error-buffer "\n")))

(cl-defstruct cmark--pathologicalTest
  name
  input
  expected)

(defun cmark--pathologicalTest (error-buffer testcase res converter)
  (cmark--cursor-write error-buffer
                       (concat (cmark--pathologicalTest-name testcase) " "))
  (let ((start-time (current-time))
        (actual (funcall converter (cmark--pathologicalTest-input testcase))))
    (if (equal actual (cmark--pathologicalTest-expected testcase))
        (progn
          (setcar res (1+ (car res)))
          (cmark--cursor-green error-buffer)
          (cmark--cursor-write error-buffer "✓")
          (cmark--cursor-reset error-buffer))
      (cmark--cursor-red error-buffer)
      (cmark--cursor-write error-buffer "✘\n")
      (cmark--cursor-cyan error-buffer)
      (cmark--cursor-write error-buffer "=== markdown ===============\n")
      (cmark--cursor-write error-buffer
                           (cmark--showSpaces
                            (cmark--pathologicalTest-input testcase)))
      (cmark--cursor-write error-buffer "=== expected ===============\n")
      (cmark--cursor-write error-buffer
                           (cmark--showSpaces
                            (cmark--pathologicalTest-expected testcase)))
      (cmark--cursor-write error-buffer "=== got ====================\n")
      (cmark--cursor-write error-buffer (cmark--showSpaces actual))
      (cmark--cursor-write error-buffer "\n")
      (cmark--cursor-reset error-buffer)
      (setcdr res (1+ (cdr res))))
    (cmark--cursor-write error-buffer "  elapsed time ")
    (cmark--cursor-write error-buffer
                         (number-to-string
                          (time-to-seconds
                           (time-since start-time))))
    (cmark--cursor-write error-buffer "\n")))

(defun cmark-run-test-cases ()
  (interactive)
  (let ((error-buffer (if noninteractive nil (cmark--setup-error-buffer)))
        (writer (cmark-create-HtmlRenderer))
        (reader (cmark-create-Parser))
        (readerSmart (cmark-create-Parser (make-cmark-options :smart t)))
        (results (cons 0 0)))
    (when (not noninteractive)
      (with-current-buffer error-buffer
        (setq-local cmark--test-color-start-marker (make-marker))))
    (cmark--specTests
     error-buffer
     (concat (file-name-as-directory cmark--test-basedir) "spec.txt")
     results
     (lambda (z)
       (cmark-HtmlRenderer-render writer (cmark-Parser-parse reader z))))

    (cmark--specTests
     error-buffer
     (concat (file-name-as-directory cmark--test-basedir) "smart_punct.txt")
     results
     (lambda (z)
       (cmark-HtmlRenderer-render writer
                                   (cmark-Parser-parse readerSmart z))))

    (cmark--specTests
     error-buffer
     (concat (file-name-as-directory cmark--test-basedir) "regression.txt")
     results
     (lambda (z)
       (cmark-HtmlRenderer-render writer (cmark-Parser-parse reader z))))

    ;; pathological cases
    (cmark--cursor-write error-buffer "Pathological cases:\n")

    (let ((cases (list
                  (make-cmark--pathologicalTest
                   :name "alternate line endings"
                   :input "- a\n- b\r- c\r\n- d"
                   :expected "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d</li>\n</ul>\n")
                  (make-cmark--pathologicalTest
                   :name "U+0000 in input"
                   :input "abc\u0000xyz\u0000\n"
                   :expected "<p>abc\ufffdxyz\ufffd</p>\n"))))
      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat "nested strong emph " (number-to-string x) " deep")
            :input (concat
                    (cmark--repeat "*a **a " x)
                    "b"
                    (cmark--repeat " a** a*" x))
            :expected (concat
                       "<p>"
                       (cmark--repeat "<em>a <strong>a " x)
                       "b"
                       (cmark--repeat " a</strong> a</em>" x)
                       "</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat
                   (number-to-string x)
                   " emph closers with no openers")
            :input (cmark--repeat "a_ " x)
            :expected (concat "<p>" (cmark--repeat "a_ " (1- x)) "a_</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat
                   (number-to-string x)
                   " emph openers with no closers")
            :input (cmark--repeat "_a " x)
            :expected (concat "<p>" (cmark--repeat "_a " (1- x)) "_a</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat
                   (number-to-string x)
                   " link closers with no openers")
            :input (cmark--repeat "a] " x)
            :expected (concat "<p>" (cmark--repeat "a] " (1- x)) "a]</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat
                   (number-to-string x)
                   " link openers with no closers")
            :input (cmark--repeat "[a " x)
            :expected (concat "<p>" (cmark--repeat "[a " (1- x)) "[a</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat
                   (number-to-string x)
                   " link openers and emph closers")
            :input (cmark--repeat "[ a_ " x)
            :expected (concat
                       "<p>"
                       (cmark--repeat "[ a_ " (1- x))
                       "[ a_</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat
                   (number-to-string x)
                   " mismatched openers and closers")
            :input (cmark--repeat "*a_ " x)
            :expected (concat "<p>" (cmark--repeat "*a_ " (1- x)) "*a_</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat (number-to-string x) " pattern [ (](")
            :input (cmark--repeat "[ (](" x)
            :expected (concat "<p>" (cmark--repeat "[ (](" x) "</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat "nested brackets " (number-to-string x) " deep")
            :input (concat (cmark--repeat "[" x) "a" (cmark--repeat "]" x))
            :expected (concat
                       "<p>"
                       (cmark--repeat "[" x)
                       "a"
                       (cmark--repeat "]" x)
                       "</p>\n"))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat "nested block quote " (number-to-string x) " deep")
            :input (concat (cmark--repeat "> " x) "a\n")
            :expected (concat
                       (cmark--repeat
                        "<blockquote>\n"
                        x)
                       "<p>a</p>\n"
                       (cmark--repeat
                        "</blockquote>\n"
                        x)))
           cases)
          (setq x (* 10 x))))

      (let ((x 1000))
        (while (<= x 10000)
          (push
           (make-cmark--pathologicalTest
            :name (concat "[\\\\... " (number-to-string x) " deep")
            :input (concat "[" (cmark--repeat "\\" x) "\n")
            :expected (concat "<p>" "[" (cmark--repeat "\\" (/ x 2)) "</p>\n"))
           cases)
          (setq x (* 10 x))))

      ;; Commented out til we have a fix...
      ;; see https://github.com/commonmark/commonmark.js/issues/129
      ;; (let ((x 1000))
      ;;   (while (<= x 10000)
      ;;     (push
      ;;      (make-cmark--pathologicalTest
      ;;       :name (concat "[]( " x " deep")
      ;;       :input (concat (cmark--repeat "[](" x) "\n")
      ;;       :expected (concat "<p>" (cmark--repeat "[](" x) "</p>\n")
      ;;       cases)
      ;;      (setq x (* 10 x))))

      (dolist (case (reverse cases))
        (cmark--pathologicalTest
         error-buffer
         case
         results
         (lambda (z)
           (cmark-HtmlRenderer-render
            writer
            (cmark-Parser-parse reader z))))))

    (cmark--cursor-write error-buffer "\n")
    (cmark--cursor-write error-buffer
                         (concat
                          (number-to-string (car results))
                          " tests passed, "
                          (number-to-string (cdr results))
                          " failed.\n"))
    (cmark--cursor-reset error-buffer))
  t)

(provide 'cmark-run-test-cases)

;;; cmark-test.el ends here
