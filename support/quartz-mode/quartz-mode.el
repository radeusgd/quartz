;;; quartz-mode.el --- sample major mode for editing LSL. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017, by you

;; Author: your name ( your email )
;; Version: 2.0.13
;; Created: 26 Jun 2015
;; Keywords: languages
;; Homepage: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq quartz-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("def" "defop" "if" "then" "else" "case" "of" "return" "val"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            ;; (x-types-regexp "\\<[A-Z]\\w*\\>")
            (x-constants-regexp "\\d+")
            (x-values-regexp "\\<[A-Za-z][A-Za-z0-9]*\\>")
            )
        `(
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; ("\\(def\\) +\\([a-z][A-Za-z0-9']*\\)" (1 font-lock-keyword-face) (2 font-lock-function-name-face))
          ;; ("\\<self\\>" . font-lock-negation-char-face)
          (,x-values-regexp . font-lock-variable-name-face)
          ;; (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode quartz-mode python-mode "Quartz"
  "Major mode for editing Quartz"

  ;; code for syntax highlighting
  (setq comment-start "//")
  (setq font-lock-defaults '((quartz-font-lock-keywords)))
  ;; (setq indent-line-function indent-relative)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.quartz\\'" . quartz-mode))

;; add the mode to the `features' list
(provide 'quartz-mode)

;;; quartz-mode.el ends here
