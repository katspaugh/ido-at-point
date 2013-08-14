;;; ido-at-point --- ido-style completion-at-point -*- lexical-binding: t; -*-

;; Author: katspaugh
;; URL: https://github.com/katspaugh/ido-at-point
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "1"))

;;; Code:

(require 'cl-lib)
(require 'ido)

(defun ido-at-point-insert (start end completion)
  "Replaces text in buffer from START to END with COMPLETION."
  (goto-char start)
  (delete-region start end)
  (insert completion))

(defun ido-at-point-complete (start end collection &optional predicate)
  "Completion for symbol at point using `ido-completing-read'."
  (let* ((input (buffer-substring start end))
         (choices (all-completions input collection predicate)))
    (if (eq 1 (length choices))
        (ido-at-point-insert start end (car choices))
      ;; timer to prevent "error in process filter" with async completions
      (run-with-idle-timer
       0 nil (lambda ()
               (let* ((sorted (cl-sort choices 'string-lessp :key 'downcase))
                      (comp (ido-completing-read "" sorted nil nil input)))
                 (when comp
                   (ido-at-point-insert start end comp))))))))

(add-to-list 'completion-in-region-functions
             (lambda (next-fun &rest args)
               (apply 'ido-at-point-complete args)))

(provide 'ido-at-point)
;;; ido-at-point.el ends here
