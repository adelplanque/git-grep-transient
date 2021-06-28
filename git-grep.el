;;; git-grep.el ---
;;
;; Copyright (C) 2021-2021, Alain Delplanque
;; Author: Alain Delplanque
;; URL: https://github.com/adelplanque/git-grep
;; Keywords:
;; Version: 0.1
;; Package-Requires:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'magit)
(require 'compile)
(require 'symbol-overlay)

(defvar git-grep-values-alist nil
  "alist of parameters by git repository")

(defvar git-grep-hit-face compilation-info-face
  "Face name to use for grep hits.")

(defconst git-grep-mode-filename-alist
  '((python-mode . "*.py")
    (emacs-lisp-mode . "*.el")
    ))

(define-compilation-mode git-grep-mode "Git-Grep"
  "Git-Grep search results."
  (let ((smbl 'git-grep)
        (pttrn '("^\\(\\(?:[^:\n]+?:\\)?[^:\n]+?\\):\\([0-9]+\\):\\([0-9]+\\):"
                 1 2 3)))
    (setq-local truncate-lines t
                compilation-disable-input t
                compilation-error-regexp-alist (list smbl)
                compilation-error-regexp-alist-alist (list (cons smbl pttrn))
                compilation-process-setup-function 'git-grep-mode-setup
                compilation-error-face git-grep-hit-face)))

(defun git-grep-mode-setup ()
  "Setup compilation variables and buffer for `git-grep'.
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

(defun git-grep-compilation-find-file (orig-fun marker filename directory &rest formats)
  "Wraps `compilation-find-file' to open file with magit when revision is present"
  (if (not (derived-mode-p 'git-grep-mode))
      (apply orig-fun marker filename directory formats)
    (let ((parts (split-string filename ":")))
      (let ((rev (car parts))
            (filename (car (cdr parts))))
        (if (not filename)
            (apply orig-fun marker rev directory formats)
          (magit-find-file rev filename))))))

(advice-add 'compilation-find-file :around #'git-grep-compilation-find-file)

(defun git-grep-default-for-read ()
  (unless (git-grep-use-region-p)
    (thing-at-point 'symbol)))

(defun git-grep-use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defclass git-grep-values ()
  ((directory :initarg :directory :initform nil)
   (filename :initarg :filename :initform nil)
   (revision :initarg :revision :initform nil)
   (expression :initarg :expression :initform nil)))

(defclass git-grep-variable (transient-variable)
  ((unset-value :initarg :unset-value :initform "unset")))

(cl-defmethod transient-init-value ((obj git-grep-variable))
  (let ((values (oref transient--prefix value))
        (var-name (oref obj variable)))
    (oset obj value (eieio-oref values var-name))))

(cl-defmethod transient-format-value ((obj git-grep-variable))
  (let ((value (oref obj value)))
    (if value
        (propertize value 'face 'transient-value)
      (propertize (oref obj unset-value) 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-set ((obj git-grep-variable) value)
  (oset obj value value)
  (let ((values (oref transient--prefix value))
        (var-name (oref obj variable)))
    (eieio-oset values var-name value)))

(cl-defmethod transient-infix-value ((obj transient-variable))
  `(,(oref obj variable) . ,(oref obj value)))

(defun git-grep-init-value (obj)
  (let ((toplevel (magit-toplevel)))
    (if (not toplevel) (error "Not in a git repository"))
    (if (not (assoc toplevel git-grep-values-alist))
        (let* ((values (git-grep-values :directory toplevel)))
          (push `(,toplevel . ,values) git-grep-values-alist)))
    (let ((values (cdr (assoc toplevel git-grep-values-alist)))
          (default (git-grep-default-for-read)))
      (if default (oset values expression default))
      (if (not (oref values filename))
          (oset values filename
                (cdr (assoc (with-current-buffer (or (buffer-base-buffer)
                                                     (current-buffer)) major-mode)
                            git-grep-mode-filename-alist))))
      (oset obj value values))))

(defun git-grep-read-expression (prompt initial-input history)
  (let* ((default (git-grep-default-for-read))
         (prompt (if default (format "Search (%s): " default) "Search: "))
         (pattern (read-string prompt nil)))
    (if (string= "" pattern) default pattern)))

(defun git-grep-read-directory (prompt initial-input history)
  (let ((directory (read-directory-name "Root directory: "))
        (toplevel (magit-toplevel)))
    (if (string-prefix-p ".." (file-relative-name directory toplevel)) toplevel directory)))

(defun git-grep-read-filename (prompt initial-input history)
  (let* ((mode (with-current-buffer (or (buffer-base-buffer) (current-buffer)) major-mode))
         (default (cdr (assoc mode git-grep-mode-filename-alist)))
         (prompt (if default (format "Filename pattern (%s): " default) "Filename pattern: "))
         (pattern (read-from-minibuffer prompt)))
    (if (string= "" pattern) default pattern)))

(defun git-grep-read-revision (prompt initial-input history)
  (let ((pseudo-revs '("{worktree}" "{index}")))
    (magit-completing-read "Search in revision: "
                           (append pseudo-revs
                                   (magit-list-refnames nil t))
                           nil nil nil 'history
                           (or (magit-branch-or-commit-at-point)
                               (magit-get-current-branch)))))

(transient-define-infix git-grep-expression-infix ()
  :description "Search expression"
  :class 'git-grep-variable
  :key "e"
  :variable 'expression
  :reader 'git-grep-read-expression
  :always-read t
  )

(transient-define-infix git-grep-directory-infix ()
  :description "Search in directory"
  :class 'git-grep-variable
  :key "d"
  :variable 'directory
  :reader 'git-grep-read-directory
  :always-read t
  )

(transient-define-infix git-grep-filename-infix ()
  :description "Search in files"
  :class 'git-grep-variable
  :key "f"
  :variable 'filename
  :reader 'git-grep-read-filename
  )

(transient-define-infix git-grep-revision-infix ()
  :description "Search in revision"
  :class 'git-grep-variable
  :key "r"
  :variable 'revision
  :reader 'git-grep-read-revision
  :unset-value "worktree"
  )

(defun git-grep-run-transient (&rest args)
  (interactive (transient-args 'git-grep))
  (let-alist args
    (if (not .expression) (message "Nothing to search")
      (let* ((default-directory .directory)
             (cmd (append (list "git" "--no-pager" "grep" "-n" "--column"
                                .expression .revision)
                          (if .filename (list "--" (shell-quote-argument .filename)))))
             (buf (compilation-start (mapconcat 'identity cmd " ") 'git-grep-mode)))
        (with-current-buffer buf (progn (symbol-overlay-remove-all)
                                        (symbol-overlay-put-all .expression nil)))))))


;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(transient-define-prefix git-grep ()
  "Git-Grep"
  :info-manual "Search text with git-grep"
  :init-value 'git-grep-init-value
  ["Arguments"
   (git-grep-expression-infix)
   (git-grep-directory-infix)
   (git-grep-filename-infix)
   (git-grep-revision-infix)]
  ["Commands"
   ("c" "run"     git-grep-run-transient)
   ("q" "Quit"    transient-quit-one)])

(provide 'git-grep)
