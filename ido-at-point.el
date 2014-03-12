;;; ido-at-point.el --- ido-style completion-at-point -*- lexical-binding: t; -*-

;; Copyright (C) 2013 katspaugh

;; Author: katspaugh
;; Keywords: convenience, abbrev
;; URL: https://github.com/katspaugh/ido-at-point
;; Version: 0.0.4
;; Package-Requires: ((emacs "24.4"))

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

(defun ido-at-point-complete (start end collection &optional predicate)
  "Completion for symbol at point using `ido-completing-read'."
  (let ((comps (completion-all-completions
                (buffer-substring-no-properties start end)
                collection predicate (- end start))))
    ;; No candidates
    (if (null comps)
        (message "No matches")
      (let* ((first (car comps))
             (common-len (ido-at-point-common-length first)))
        ;; Remove the last non-nil element of a possibly improper list
        (nconc comps nil)
        (if (null (cdr comps))
            ;; Single candidate
            (ido-at-point-insert start end common-len first)
          ;; Many candidates
          (let ((common (substring-no-properties first 0 common-len)))
            (ido-at-point-do-complete start end common-len comps common)))))))

(defun ido-at-point-do-complete (start end common-len comps common)
  (run-with-idle-timer
   0 nil
   (lambda ()
     (let ((choice (ido-at-point-read comps common)))
       (when (stringp choice)
         (ido-at-point-insert start end common-len choice))))))

(defun ido-at-point-read (comps common)
  (ido-completing-read "" comps nil t common))

(defun ido-at-point-common-length (candidate)
  ;; Completion text should have a property of
  ;; `(face completions-common-part)'
  ;; which we'll use to determine whether the completion
  ;; contains the common part.
  (let ((pos 0)
        (len (length candidate)))
    (while (and (<= pos len)
                (let ((prop (get-text-property pos 'face candidate)))
                  (not (eq 'completions-common-part
                           (if (listp prop) (car prop) prop)))))
      (setq pos (1+ pos)))
    (if (< pos len)
        (or (next-single-property-change pos 'face candidate) len)
      0)))

(defun ido-at-point-insert (start end common-part-length completion)
  "Replaces text in buffer from END back to COMMON-PART-LENGTH
with COMPLETION."
  (let ((reg-start (- end common-part-length)))
    (goto-char end)
    (delete-region (max start reg-start) end)
    (insert completion)))

(let ((original-fn #'completion--in-region))
  (defun ido-at-point-completion-in-region (&rest args)
    (apply (if (window-minibuffer-p)
               original-fn #'ido-at-point-complete) args))
  (defun ido-at-point-mode-set (enable)
    (if enable
        (progn
          (setq original-fn completion-in-region-function)
          (setq completion-in-region-function
                #'ido-at-point-completion-in-region))
      (setq completion-in-region-function original-fn))))

;;;###autoload
(define-minor-mode ido-at-point-mode
  "Global minor mode to use ido for `completion-at-point'.

When called interactively, toggle `ido-at-point-mode'.  With
prefix ARG, enable `ido-at-point-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ido-at-point-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ido-at-point-mode'.  Otherwise behave as if called
interactively.

With `ido-at-point-mode' use ido for `completion-at-point'."
  :variable ((eq completion-in-region-function
                 #'ido-at-point-completion-in-region)
             .
             ido-at-point-mode-set))

(provide 'ido-at-point)

;;; ido-at-point.el ends here
