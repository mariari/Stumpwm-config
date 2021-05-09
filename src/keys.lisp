(defpackage #:stump-config.keys
  (:documentation "Provides the key setting configuration for stumpwm")
  (:use #:common-lisp #:stumpwm))

(in-package #:stump-config.keys)

(set-prefix-key (kbd "s-x"))

(defun format-command (command)
  "we need to format the command, if it's an application format it
like a string, otherwise just leave it be"
  (if (listp command)
      (format nil "~{~a~^ ~}" command)
      command))

(defmacro defun-key (map key command)
  `(define-key ,map ,key (format-command ',command)))


(defun-key *root-map* (kbd "z")
  *exchange-window-map*)

(defun-key *root-map* (kbd "C-z") (echo "Z..."))

(defun-key *top-map* (kbd "s-`") (select-window-by-number 0))
(defun-key *top-map* (kbd "s-1") (select-window-by-number 1))
(defun-key *top-map* (kbd "s-2") (select-window-by-number 2))
(defun-key *top-map* (kbd "s-3") (select-window-by-number 3))
(defun-key *top-map* (kbd "s-4") (select-window-by-number 4))
(defun-key *top-map* (kbd "s-5") (select-window-by-number 5))

(defun-key *top-map* (kbd "s-F1") (gselect 1))
(defun-key *top-map* (kbd "s-F2") (gselect 2))
(defun-key *top-map* (kbd "s-F3") (gselect 3))
(defun-key *top-map* (kbd "s-F4") (gselect 4))
(defun-key *top-map* (kbd "s-F5") (gselect 5))


;; vim movement keys
(defun-key *top-map* (kbd "s-h") (move-focus :left))
(defun-key *top-map* (kbd "s-j") (move-focus :down))
(defun-key *top-map* (kbd "s-k") (move-focus :up))
(defun-key *top-map* (kbd "s-l") (move-focus :right))
