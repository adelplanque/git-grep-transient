;;; git-grep-transient.el --- Search for text using git grep command -*- lexical-binding: t -*-
;;
;; Copyright (C) 2021-2021, Alain Delplanque

;; Maintainer: Alain Delplanque <alaindelplanque@mailoo.org>
;; Author: Alain Delplanque <alaindelplanque@mailoo.org>
;; URL: https://github.com/adelplanque/git-grep-transient
;; Keywords: git tools vc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.3.0") (transient "0.6.0") (symbol-overlay "4.2"))

;; This file is NOT part of GNU Emacs.

;;; Licence:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; git-grep-transient.el allows pattern searching using the git grep command.
;; It works by using the transient package for selection of search options,
;; pattern, directory, file type and reference.

;; Usage:

;;     (require 'git-grep-transient)

;; Then M-x git-grep-transient to start command

;;; Code:

(require 'compile)
(require 'magit)
(require 'symbol-overlay)
(require 'transient)

(defvar git-grep-transient--values-alist nil
  "Alist of parameters by git repository.")

(defvar git-grep-transient--hit-face compilation-info-face
  "Face name to use for grep hits.")

(defconst git-grep-transient--mode-filename-alist
  '((python-mode . "*.py")
    (emacs-lisp-mode . "*.el")))

(define-compilation-mode git-grep-transient--mode "Git-Grep"
  "Compilation mode for Git-Grep search results."
  (let ((smbl 'git-grep-transient)
        (pttrn '("^\\(\\(?:[^:\n]+?:\\)?[^:\n]+?\\):\\([0-9]+\\):\\([0-9]+\\):"
                 1 2 3)))
    (setq-local truncate-lines t)
    (setq-local compilation-disable-input t)
    (setq-local compilation-error-regexp-alist (list smbl))
    (setq-local compilation-error-regexp-alist-alist (list (cons smbl pttrn)))
    (setq-local compilation-process-setup-function #'git-grep-transient--mode-setup)
    (setq-local compilation-error-face git-grep-transient--hit-face)))

(defun git-grep-transient--mode-setup ()
  "Setup compilation variables and buffer for `git-grep-transient'.
Set up `compilation-exit-message-function'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (if (eq status 'exit)
             (cond ((and (zerop code) (buffer-modified-p))
                    '("finished (matches found)\n" . "matched"))
                   ((not (buffer-modified-p))
                    '("finished with no matches found\n" . "no match"))
                   (t
                    (cons msg code)))
           (cons msg code)))))

(defun git-grep-transient--compilation-find-file
    (orig-fun marker filename directory &rest formats)
  "Wrap `compilation-find-file' to open file at a specific revision.
ORIG-FUN MARKER FILENAME DIRECTORY FORMATS are then arguments of the
`compilation-find-file' function."
  (if (not (derived-mode-p 'git-grep-transient--mode))
      (apply orig-fun marker filename directory formats)
    (let ((parts (split-string filename ":")))
      (let ((rev (car parts))
            (filename (car (cdr parts))))
        (if (not filename)
            (apply orig-fun marker rev directory formats)
          (magit-find-file rev filename))))))

(advice-add 'compilation-find-file :around #'git-grep-transient--compilation-find-file)

(defun git-grep-transient--default-for-read ()
  "Determine the default value of the expression to search for.
This value is based on the position in the buffer."
  (unless (git-grep-transient--use-region-p)
    (let ((litteral (thing-at-point 'symbol)))
      (set-text-properties 0 (length litteral) nil litteral)
      litteral)))

(defun git-grep-transient--use-region-p ()
  "When to use region as a search expression."
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defclass git-grep-transient--values ()
  ((directory :initarg :directory :initform nil)
   (filename :initarg :filename :initform nil)
   (revision :initarg :revision :initform nil)
   (expression :initarg :expression :initform nil))
  "Parameters to call the git grep command.")

(defclass git-grep-transient--variable (transient-variable)
  ((unset-value :initarg :unset-value :initform "unset"))
  "Subclass of function `transient-variable' that manages history.")

(cl-defmethod transient-init-value ((obj git-grep-transient--variable))
  "Overriding the `transient-init-value' method.
The value of object OBJ is initialized from the last used value in the same git
repository."
  (let ((values (oref transient--prefix value))
        (var-name (oref obj variable)))
    (oset obj value (eieio-oref values var-name))))

(cl-defmethod transient-format-value ((obj git-grep-transient--variable))
  "Return the propertized vulue string for `git-grep-transient--variable' OBJ."
  (let ((value (oref obj value)))
    (if value
        (propertize value 'face 'transient-value)
      (propertize (oref obj unset-value) 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-set ((obj git-grep-transient--variable) value)
  "Set VALUE for `git-grep-transient--variable' OBJ."
  (oset obj value value)
  (let ((values (oref transient--prefix value))
        (var-name (oref obj variable)))
    (eieio-oset values var-name value)))

(cl-defmethod transient-infix-value ((obj transient-variable))
  "Return the value of OBJ's `value' slot."
  `(,(oref obj variable) . ,(oref obj value)))

(defun git-grep-transient--init-value (obj)
  "Initialize value for `git-grep-transient--variable' OBJ."
  (let ((toplevel (magit-toplevel)))
    (if (not toplevel) (user-error "Not in a git repository"))
    (if (not (assoc toplevel git-grep-transient--values-alist))
        (let* ((values (git-grep-transient--values :directory toplevel)))
          (push `(,toplevel . ,values) git-grep-transient--values-alist)))
    (let ((values (cdr (assoc toplevel git-grep-transient--values-alist)))
          (default (git-grep-transient--default-for-read)))
      (if default (oset values expression default))
      (if (not (oref values filename))
          (oset values filename
                (cdr (assoc (with-current-buffer (or (buffer-base-buffer)
                                                     (current-buffer)) major-mode)
                            git-grep-transient--mode-filename-alist))))
      (oset obj value values))))

(defun git-grep-transient--read-expression (&rest _args)
  "Ask the user for an expression to search for."
  (let* ((default (git-grep-transient--default-for-read))
         (prompt (if default (format "Search (%s): " default) "Search: "))
         (pattern (read-string prompt nil)))
    (if (string= "" pattern) default pattern)))

(defun git-grep-transient--read-directory (&rest _args)
  "Ask user for a directory."
  (let ((directory (read-directory-name "Root directory: "))
        (toplevel (magit-toplevel)))
    (if (string-prefix-p ".." (file-relative-name directory toplevel)) toplevel directory)))

(defun git-grep-transient--read-filename (&rest _args)
  "Ask user for a filename or filename pattern."
  (let* ((mode (with-current-buffer (or (buffer-base-buffer) (current-buffer)) major-mode))
         (default (cdr (assoc mode git-grep-transient--mode-filename-alist)))
         (prompt (if default (format "Filename pattern (%s): " default) "Filename pattern: "))
         (pattern (read-from-minibuffer prompt)))
    (if (string= "" pattern) default pattern)))

(defun git-grep-transient--read-revision (&rest _args)
  "Ask user for a git revision."
  (let ((pseudo-revs '("{worktree}" "{index}")))
    (magit-completing-read "Search in revision: "
                           (append pseudo-revs
                                   (magit-list-refnames nil t))
                           nil nil nil 'history
                           (or (magit-branch-or-commit-at-point)
                               (magit-get-current-branch)))))

(transient-define-infix git-grep-transient--expression-infix ()
  :description "Search expression"
  :class 'git-grep-transient--variable
  :key "e"
  :variable 'expression
  :reader #'git-grep-transient--read-expression
  :always-read t)

(transient-define-infix git-grep-transient--directory-infix ()
  :description "Search in directory"
  :class 'git-grep-transient--variable
  :key "d"
  :variable 'directory
  :reader #'git-grep-transient--read-directory
  :always-read t)

(transient-define-infix git-grep-transient--filename-infix ()
  :description "Search in files"
  :class 'git-grep-transient--variable
  :key "f"
  :variable 'filename
  :reader #'git-grep-transient--read-filename)

(transient-define-infix git-grep-transient--revision-infix ()
  :description "Search in revision"
  :class 'git-grep-transient--variable
  :key "r"
  :variable 'revision
  :reader #'git-grep-transient--read-revision
  :unset-value "worktree")

(defun git-grep-transient--run (&rest args)
  "Run.the git grep command.
ARGS are arguments provided by `git-grep-transient'."
  (interactive (transient-args 'git-grep-transient))
  (let-alist args
    (if (not .expression) (user-error "Nothing to search")
      (let* ((default-directory .directory)
             (cmd (append (list "git" "--no-pager" "grep" "-n" "--column"
                                (shell-quote-argument .expression)
                                (when .revision (shell-quote-argument .revision)))
                          (if .filename (list "--" (shell-quote-argument .filename)))))
             (buf (compilation-start (mapconcat #'identity cmd " ") #'git-grep-transient--mode)))
        (with-current-buffer buf
          (symbol-overlay-remove-all)
          (setq symbol-overlay-keywords-alist nil)
          (symbol-overlay-put-all .expression nil))))))

;;;###autoload (autoload 'git-grep-transient "git-grep-transient" nil t)
(transient-define-prefix git-grep-transient ()
  "Search text with git grep command."
  :info-manual "Search text with git-grep-transient"
  :init-value #'git-grep-transient--init-value
  ["Arguments"
   (git-grep-transient--expression-infix)
   (git-grep-transient--directory-infix)
   (git-grep-transient--filename-infix)
   (git-grep-transient--revision-infix)]
  ["Commands"
   ("c" "run"     git-grep-transient--run)
   ("q" "Quit"    transient-quit-one)])

(provide 'git-grep-transient)
;;; git-grep-transient.el ends here
