;;; flymake-transient-keys.el --- Flymake backend for linting transient prefixes -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/flymake-transient-keys
;; Version: 0.2.0
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

;; Flymake backends to check transient prefixes for duplicated keys.

;; `flymake-transient-keys' - To check all transient prefixes in the buffer.

;; (add-hook 'emacs-lisp-mode-hook #'flymake-transient-keys)


;; `flymake-transient-keys-current' - To check only transient prefix at point

;; (add-hook 'emacs-lisp-mode-hook #'flymake-transient-keys-current)


;;; Code:


(declare-function flymake-start "flymake")
(declare-function flymake-make-diagnostic "flymake")

(defun flymake-transient-keys-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (with-syntax-table emacs-lisp-mode-syntax-table
    (when-let ((str-start (nth 8 (syntax-ppss (point)))))
      (goto-char str-start))
    (let ((init-pos (point))
          (pos)
          (count (if (> n 0) n (- n))))
      (while
          (and (not (= count 0))
               (when-let ((end (ignore-errors
                                 (funcall fn (if
                                                 (> n 0) 1
                                               -1))
                                 (point))))
                 (unless (or (= end
                                (or pos init-pos))
                             (nth 4 (syntax-ppss (point)))
                             (and (looking-at ";")
                                  (nth 4 (syntax-ppss (1+ (point))))))
                   (setq pos end))))
        (setq count
              (1- count)))
      (if (= count 0)
          pos
        (goto-char init-pos)
        nil))))


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

(defun flymake-transient-keys-lint-re-search-backward (regexp &optional bound
                                                              noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (flymake-transient-keys-lint-re-search-forward regexp bound noerror
                                                 (if count
                                                     (- count) -1)))

(defun flymake-transient-keys-find-dups-keys (lists)
  "Return filtered LISTS with duplicated keys."
  (setq lists (mapcar 'car lists))
  (let ((dups)
        (uniq-keys)
        (key))
    (while (setq key (pop lists))
      (let ((common-pref))
        (cond ((member key lists)
               (setq dups (push key dups))
               (setq lists (remove key lists)))
              ((setq common-pref (unless (or (string-prefix-p "M-" key)
                                             (string-prefix-p "C-" key))
                                   (or (seq-filter
                                        (apply-partially #'string-prefix-p
                                                         key)
                                        lists)
                                       (seq-filter
                                        (apply-partially #'string-prefix-p
                                                         key)
                                        uniq-keys))))
               (when (or (string= key "M")
                         (string= key "C"))
                 (setq common-pref (seq-remove
                                    (lambda (it)
                                      (or (string-prefix-p "M-" it)
                                          (string-prefix-p "C-" it)))
                                    common-pref)))
               (when common-pref
                 (setq dups (nconc dups common-pref))
                 (setq dups (push key dups))))
              (t (push key uniq-keys)))))
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
         (eq (car-safe sexp) 'transient-define-prefix)
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
    (let ((case-fold-search nil)
          (problems))
      (while (flymake-transient-keys-lint-re-search-backward
              "\\_<\\(transient-define-prefix\\)\\_>"
              nil t 1)
        (ignore-errors (backward-up-list 1))
        (when-let ((dups
                    (flymake-transient-keys-check-transient-at-point)))
          (setq problems (nconc problems
                                dups))))
      problems)))

(defun flymake-transient-keys--search-heads (keys limit)
  "Search for KEYS from current point until LIMIT."
  (let ((problems))
    (dolist (key keys)
      (save-excursion
        (let ((re (if (member key '("M" "C"))
                      (concat "[(]\"" (regexp-quote key) "[^-]")
                    (concat "[(]\"" (regexp-quote key))))
              (case-fold-search nil)
              (found))
          (while (setq found (re-search-forward re limit t 1))
            (push (cons (- found (length key)) found) problems)))))
    problems))

(defun flymake-transient-keys-check-transient-at-point ()
  "Check current buffer for duplicate keys in transient prefixes."
  (when-let* ((keys (flymake-transient-keys-find-dups-keys
                     (flymake-transient-keys-parse-sexp
                      (sexp-at-point))))
              (end (save-excursion
                     (forward-sexp 1)
                     (point))))
    (flymake-transient-keys--search-heads keys
                                          end)))

(defun flymake-transient-keys-check-current-transient ()
  "Check current buffer for duplicate keys in transient prefixes."
  (let ((result))
    (save-excursion
      (while (and (not
                   (setq result
                         (flymake-transient-keys-check-transient-at-point)))
                  (flymake-transient-keys-move-with 'backward-up-list))))
    result))

(defun flymake-transient-keys-make-diagnostic (problems report-fn)
  "Make flymake diagnostic from list of PROBLEMS and call REPORT-FN directly."
  (let ((buff (current-buffer)))
    (funcall report-fn (mapcar
                        (lambda (it)
                          (flymake-make-diagnostic buff
                                                   (car it)
                                                   (cdr it)
                                                   :error
                                                   "Duplicated key"))
                        problems))))

(defun flymake-transient-keys-check-current (report-fn &rest _args)
  "Check duplicated keys for transient at point and call REPORT-FN.
Use `flymake-transient-keys-current' to add this to
`flymake-diagnostic-functions'."
  (flymake-transient-keys-make-diagnostic
   (flymake-transient-keys-check-current-transient)
   report-fn))

(defun flymake-transient-keys-check (report-fn &rest _args)
  "Check whole buffer for duplicated keys in `define-transient-prefix' forms.
Use `flymake-transient-keys' to add this to `flymake-diagnostic-functions'.
Calls REPORT-FN directly."
  (flymake-transient-keys-make-diagnostic (flymake-transient-keys-search-dups)
                                          report-fn))

;;;###autoload
(defun flymake-transient-keys-current ()
  "Check `define-transient-prefix' form at point for duplicated keys.
Add `flymake-transient-keys-check' to flymake diagnostic and run flymake."
  (interactive)
  (require 'flymake)
  (add-hook 'flymake-diagnostic-functions
            #'flymake-transient-keys-check-current nil t)
  (flymake-start))

;;;###autoload
(defun flymake-transient-keys ()
  "Check whole buffer for duplicated keys in `define-transient-prefix' forms.
Add `flymake-transient-keys-check' to flymake diagnostic and run flymake."
  (interactive)
  (require 'flymake)
  (add-hook 'flymake-diagnostic-functions #'flymake-transient-keys-check nil t)
  (flymake-start))

(provide 'flymake-transient-keys)
;;; flymake-transient-keys.el ends here