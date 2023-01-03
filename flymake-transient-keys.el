;;; flymake-transient-keys.el --- Flymake backend for linting transient prefixes -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/flymake-transient-keys
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "26.1") (flymake "1.2.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flymake backend to check transient prefixes for duplicated keys

;;; Code:


(eval-when-compile
  (require 'cl-lib))

(require 'flymake)

(declare-function flymake-diag-region "flymake")
(declare-function flymake-make-diagnostic "flymake")

(defun flymake-transient-keys-lint-re-search-backward-inner (regexp &optional
                                                           bound count)
  "This function is helper for `flymake-transient-keys-lint-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun flymake-transient-keys-lint-re-search-forward-inner (regexp &optional
                                                                   bound count)
  "This function is helper for `flymake-transient-keys-lint-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun flymake-transient-keys-lint-re-search-forward (regexp &optional bound
                                                             noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count (- count))
                #'flymake-transient-keys-lint-re-search-backward-inner)
               ((> count 0)
                #'flymake-transient-keys-lint-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defun flymake-transient-keys-lint-re-search-backward (regexp &optional
                                                     bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (flymake-transient-keys-lint-re-search-forward regexp bound noerror
                                        (if count
                                            (- count) -1)))

(defun flymake-transient-keys-find-dups (lists)
  "Return filtered LISTS with duplicated keys."
  (let ((dups)
        (head)
        (items))
    (while (setq head (pop lists))
      (let ((key (car head)))
        (when (member key items)
          (push head dups))
        (push key items)))
    dups))

(defun flymake-transient-keys-lint-vectors-to-list (vect &optional acc)
  "Return list of lists from vector VECT.
ACC is used for inner purposes."
  (cond ((vectorp vect)
         (remove nil
                 (seq-mapcat #'flymake-transient-keys-lint-vectors-to-list
                             (append
                              vect
                              nil))))
        ((and (listp vect)
              (car vect))
         (push vect acc))))

(defun flymake-transient-keys-parse-sexp (sexp)
  "Parse transient SEXP."
  (when (and
         (listp sexp)
         (car sexp)
         (eq (car sexp) 'transient-define-prefix)
         (car sexp)
         (symbolp (nth 1 sexp)))
    (let ((items (seq-take-while
                  #'vectorp
                  (reverse
                   sexp))))
      (seq-filter
       (lambda (it)
         (stringp (car it)))
       (seq-reduce (lambda (acc v)
                     (setq acc
                           (append acc
                                   (flymake-transient-keys-lint-vectors-to-list
                                    v))))
                   items '())))))

(defun flymake-transient-keys-search-dups ()
  "Check current buffer for duplicate keys in transient prefixes."
  (save-excursion
    (goto-char (point-max))
    (let ((problems)
          (case-fold-search nil))
      (while (flymake-transient-keys-lint-re-search-backward
              "\\_<\\(transient-define-prefix\\)\\_>"
              nil t 1)
        (when-let* ((sexp (progn (ignore-errors
                                   (backward-up-list 1))
                                 (sexp-at-point)))
                    (heads
                     (flymake-transient-keys-find-dups
                      (flymake-transient-keys-parse-sexp
                       sexp)))
                    (bounds (when (listp sexp)
                              (bounds-of-thing-at-point 'sexp))))
          (dolist (head heads)
            (save-excursion
              (let ((re (concat "\\_<\\("
                                (regexp-quote (car head))
                                "\\)\\_>"))
                    (case-fold-search nil))
                (while (re-search-forward re (cdr bounds) t 1)
                  (let ((col (current-column))
                        (line (line-number-at-pos)))
                    (setq col (if (>= col (length (car head)))
                                  (- col (length (car head)))
                                col))
                    (push `(,line ,col error "Duplicated key")
                          problems))))))))
      problems)))

(defun flymake-transient-keys-check (report-fn &rest _args)
  "A Flymake backend for checking duplicated keys in transient.
Use `flymake-transient-keys' to add this to `flymake-diagnostic-functions'.
Calls REPORT-FN directly."
  (let ((collection (flymake-transient-keys-search-dups)))
    (cl-loop for (line col type message) in
             collection
             for (beg . end) = (flymake-diag-region (current-buffer) line col)
             collect
             (flymake-make-diagnostic
              (current-buffer)
              beg end
              (if (eq type 'warning) :warning :error)
              message)
             into diags
             finally (funcall report-fn diags)))
  (funcall report-fn nil))

;;;###autoload
(defun flymake-transient-keys ()
  "Add `flymake-transient-keys-check' to flymake diagnostic and run flymake."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-transient-keys-check nil t)
  (flymake-mode))

(provide 'flymake-transient-keys)
;;; flymake-transient-keys.el ends here