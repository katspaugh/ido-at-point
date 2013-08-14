;;; -*- lexical-binding: t; -*-
;;; ido-completion-in-region.el --- ido-style completion-at-point.

;; Author: katspaugh
;; URL: https://github.com/katspaugh/ido-completioon-in-region
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

(eval-when-compile (require 'cl))

(defun ido-insert-completion (start end completion)
  "Replaces text in buffer from START to END with COMPLETION."
  (goto-char start)
  (delete-region start end)
  (insert completion))

(defun ido-completion-in-region (start end collection &optional predicate)
  "Makes `complete-at-point' display completions using `ido-completing-read'."
  (let* ((input (buffer-substring start end))
         (choices (all-completions input collection predicate)))
    (if (eq 1 (length choices))
        (ido-insert-completion start end (car choices))
      ;; timer to prevent "error in process filter" with async completions
      (run-with-idle-timer
       0 nil (lambda ()
               (let* ((sorted (cl-sort choices 'string-lessp :key 'downcase))
                      (comp (ido-completing-read "" sorted nil nil input)))
                 (when comp
                   (ido-insert-completion start end comp))))))))

(add-to-list 'completion-in-region-functions
             (lambda (next-fun &rest args)
               (apply 'ido-completion-in-region args)))

(provide 'ido-completion-in-region)
