;;; ido-at-point.el --- ido-style completion-at-point -*- lexical-binding: t; -*-

;; Copyright (C) 2013 katspaugh

;; Author: katspaugh
;; Keywords: convenience, abbrev
;; URL: https://github.com/katspaugh/ido-at-point
;; Version: 0.0.2
;; Package-Requires: ((emacs "24") (cl-lib "0.3"))

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

;; This package is an alternative frontend for `completion-at-point'.
;; It replaces the standard completions buffer with ido prompt.
;; Press <M-tab> or <C-M-i> to complete a symbol at point.

;;; Installation:

;; (require 'ido-at-point) ; unless installed from a package
;; (ido-at-point-mode)

;;; Code:

(require 'ido)

(defvar ido-at-point-partial t
  "If nil, don't complete partial match on the first completion attempt.")

(defun ido-at-point-insert (start end completion)
  "Replaces text in buffer from START to END with COMPLETION."
  (goto-char start)
  (delete-region start end)
  (insert completion))

(defun ido-at-point-complete (start end collection &optional predicate)
  "Completion for symbol at point using `ido-completing-read'."
  (let* ((input (buffer-substring-no-properties start end))
         (choices (all-completions input collection predicate)))
    (cond ((null choices)
           (message "No match"))
          ((null (cdr choices))
           (ido-at-point-insert start end (car choices)))
          (t
           (let ((common (try-completion input choices)))
             (if (and ido-at-point-partial
                      (stringp common) (not (string= common input)))
                 (ido-at-point-insert start end common)
               ;; timer to prevent "error in process filter"
               (run-with-idle-timer
                0 nil
                (lambda ()
                  (ido-at-point-insert
                   start end
                   (ido-completing-read "" choices nil nil input))))))))))

(defun ido-at-point-completion-in-region (&rest args)
  "See `ido-at-point-complete'."
  (apply 'ido-at-point-complete (cdr args)))

(defun ido-at-point-mode-set (enable)
  (if enable
      (add-to-list 'completion-in-region-functions
                   'ido-at-point-completion-in-region)
    (setq completion-in-region-functions
          (delq 'ido-at-point-completion-in-region
                completion-in-region-functions))))

;;;###autoload
(define-minor-mode ido-at-point-mode
  "Global minor mode to use IDO for `completion-at-point'.

When called interactively, toggle `ido-at-point-mode'.  With
prefix ARG, enable `ido-at-point-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ido-at-point-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ido-at-point-mode'.  Otherwise behave as if called
interactively.

With `ido-at-point-mode' use IDO for `completion-at-point'."
  :variable ((memq 'ido-at-point-completion-in-region
                   completion-in-region-functions)
             .
             ido-at-point-mode-set))

(provide 'ido-at-point)

;;; ido-at-point.el ends here
