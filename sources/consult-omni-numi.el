;;; consult-omni-numi.el --- Consulting numi Command -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-omni "0.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:
;; consult-omni-numi provides commands for doing calculations directly in
;; Emacs minibuffer using consult-omni as the frontend and numi-cli shell
;; commands as the backend.
;;
;; For more info on nuumi and numi-cli see the following URLs:
;; URL `https://numi.app'
;; URL `https://github.com/nikolaeu/numi'

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-numi-args "numi-cli"
  "Command line arguments for “numi-cli”.

Similar to other command line args for consult but for numi-cli.
See `consult-locate-args' for example."
  :group 'consult-omni
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--numi-preview (cand)
  "Preview function for CAND from `consult-omni-numi'."
  (ignore))

(defun consult-omni--numi-callback (cand)
  "Callback function for CAND from `consult-omni-numi'."
  (let ((result  (get-text-property 0 :title cand)))
    (kill-new result)))

(defun consult-omni--numi-filter (candidates &optional query)
  "Filter CANDIDATES from `consult-omni-numi'.

QUERY is the user input string."
  (cl-loop for candidate in candidates
           when (not (equal candidate "?"))
           collect candidate))

(cl-defun consult-omni--numi-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “numi-cli” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts)))
    (funcall #'consult-omni--async-builder (shell-quote-argument query) consult-omni-numi-args)))

;; Define the Numi source
(consult-omni-define-source "Numi"
                            :narrow-char ?N
                            :category 'consult-omni-calc
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--numi-builder
                            :filter #'consult-omni--numi-filter
                            :on-preview #'ignore
                            :on-return #'identity
                            :on-callback #'consult-omni--numi-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (executable-find "numi-cli"))
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-numi module

(provide 'consult-omni-numi)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-numi)
;;; consult-omni-numi.el ends here
