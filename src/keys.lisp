(defpackage #:stump-config.keys
  (:documentation "Provides the key setting configuration for stumpwm")
  (:use #:common-lisp #:stumpwm)
  (:export
   :frame-switch-by-number
   :repack-frame-numbers))

(in-package #:stump-config.keys)

;; ---------------------------------------------------------------
;;; General Definition Facilities
;; ---------------------------------------------------------------

(defun format-command (command)
  "we need to format the command, if it's an application format it
like a string, otherwise just leave it be"
  (if (listp command)
      (format nil "~{~a~^ ~}" command)
      command))

(defmacro defun-key (map key command)
  `(define-key ,map ,key (format-command ',command)))

;; ---------------------------------------------------------------
;;; Special commands
;; ---------------------------------------------------------------

(defcommand repack-frame-numbers () ()
  (stumpwm::group-repack-frame-numbers (current-group)))

(defcommand frame-switch-by-number (number) ((:window-number "Select: "))
  "switches frame by number"
  (let ((frame (stumpwm::frame-by-number (current-group) number)))
    (when frame
      (fselect frame))))

;; ---------------------------------------------------------------
;;; Key bindings
;; ---------------------------------------------------------------

(defparameter *modifier* "s")

(defun modifier (string)
  (concatenate 'string *modifier* "-" string))

(set-prefix-key (kbd (modifier "x")))

(defun-key *root-map* (kbd "z") stumpwm::*exchange-window-map*)

(defun-key *root-map* (kbd "C-z") (echo "Z..."))

;; Repacking
(defun-key *root-map* (kbd "C-f") (repack-frame-numbers))
(defun-key *root-map* (kbd "C-w") (repack-window-numbers))

;; ---------------------------------------------------------------
;; Number selectors
;; ---------------------------------------------------------------

;; Window selection
;; Not sure I want these as they are on the root map easily
;; (defun-key *top-map* (kbd ( modifier"`")) (select-window-by-number 0))
;; (defun-key *top-map* (kbd ( modifier"1")) (select-window-by-number 1))
;; (defun-key *top-map* (kbd ( modifier"2")) (select-window-by-number 2))
;; (defun-key *top-map* (kbd ( modifier"3")) (select-window-by-number 3))
;; (defun-key *top-map* (kbd ( modifier"4")) (select-window-by-number 4))
;; (defun-key *top-map* (kbd ( modifier"5")) (select-window-by-number 5))

(defun-key *top-map* (kbd (modifier "`")) (frame-switch-by-number 0))
(defun-key *top-map* (kbd (modifier "1")) (frame-switch-by-number 1))
(defun-key *top-map* (kbd (modifier "2")) (frame-switch-by-number 2))
(defun-key *top-map* (kbd (modifier "3")) (frame-switch-by-number 3))
(defun-key *top-map* (kbd (modifier "4")) (frame-switch-by-number 4))
(defun-key *top-map* (kbd (modifier "5")) (frame-switch-by-number 5))
(defun-key *top-map* (kbd (modifier "6")) (frame-switch-by-number 6))


(defun-key *top-map* (kbd (modifier "F1")) (gselect 1))
(defun-key *top-map* (kbd (modifier "F2")) (gselect 2))
(defun-key *top-map* (kbd (modifier "F3")) (gselect 3))
(defun-key *top-map* (kbd (modifier "F4")) (gselect 4))
(defun-key *top-map* (kbd (modifier "F5")) (gselect 5))

(defun-key *top-map* (kbd (modifier "f")) (fullscreen))

;; ---------------------------------------------------------------
;; vim movement Selectors
;; ---------------------------------------------------------------

(defun-key *top-map* (kbd (modifier "h")) (move-focus :left))
(defun-key *top-map* (kbd (modifier "j")) (move-focus :down))
(defun-key *top-map* (kbd (modifier "k")) (move-focus :up))
(defun-key *top-map* (kbd (modifier "l")) (move-focus :right))

(defun-key *top-map* (kbd (modifier "C-h")) (move-window :left))
(defun-key *top-map* (kbd (modifier "C-j")) (move-window :down))
(defun-key *top-map* (kbd (modifier "C-k")) (move-window :up))
(defun-key *top-map* (kbd (modifier "C-l")) (move-window :right))

;; ---------------------------------------------------------------
;; Other
;; ---------------------------------------------------------------

;; Frame cycling
(defun-key *root-map* (kbd "j") (prev-in-frame))
(defun-key *root-map* (kbd "k") (next-in-frame))

;; Pulling
(defun-key *root-map* (kbd "p") (pull-hidden-previous))

;; Windows Listing
(defun-key *root-map* (kbd "W") (windowlist))
(defun-key *root-map* (kbd "E") (frame-windowlist))
(defun-key *root-map* (kbd "e") (frame-windows))

;; Placing
(defun-key *root-map* (kbd "C-p") (place-existing-windows))

;; killing
(defun-key *root-map* (kbd "q") (delete))
(defun-key *root-map* (kbd "S-q") (kill))
(defun-key *root-map* (kbd "C-S-q") (quit-confirm))

;; Calling menus
(defun-key *top-map* (kbd (modifier "d")) (exec))
(defun-key *top-map* (kbd (modifier "n")) (exec "passmenu"))
