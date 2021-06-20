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

(defun git-grep-mode-get-buffer (filename &optional rev)
  (if (not rev)
      (find-file filename)
    (magit-find-file rev filename)))

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
                compilation-error-face grep-hit-face)))

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

(defun git-grep-buffer-name (mode) "Git-Grep")

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

(defun git-grep-run (pattern rev directory)
  "Internal function, run git-grep."
  (let (
        ;; (compilation-buffer-name-function 'git-grep-buffer-name)
        (default-directory directory)
        (cmd (list "git" "--no-pager" "grep" "-n" "--column" pattern rev)))
    (compilation-start (mapconcat 'identity cmd " ") 'git-grep-mode)))

(defun git-grep-default-for-read ()
  (unless (git-grep-use-region-p)
    (thing-at-point 'symbol)))

(defun git-grep-use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defun git-grep-read-pattern ()
  (let ((default (git-grep-default-for-read)))
    (let ((prompt (if default (format "Search (%s): " default) "Search: ")))
      (let ((pattern (read-string prompt nil)))
        (if (string= "" pattern) default pattern)))))

(defun git-grep-read-rev ()
  (let ((pseudo-revs '("{worktree}" "{index}")))
    (magit-completing-read "Find file from revision"
                           (append pseudo-revs
                                   (magit-list-refnames nil t))
                           nil nil nil 'magit-revision-history
                           (or (magit-branch-or-commit-at-point)
                               (magit-get-current-branch)))))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun git-grep (pattern rev directory)
  "Run git-grep"
  (interactive
   (let ((directory (let ((toplevel (magit-toplevel)))
                      (if (not toplevel) (error "Not in a git repository") toplevel)))
         (pattern (git-grep-read-pattern))
         (rev (git-grep-read-rev)))
     (list pattern rev directory)))
  (git-grep-run pattern rev directory))

(provide 'git-grep)
