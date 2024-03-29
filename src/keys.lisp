(defpackage #:stump-config.keys
  (:documentation "Provides the key setting configuration for stumpwm")
  (:use #:common-lisp #:stumpwm)
  (:export
   :frame-switch-by-number
   :repack-frame-numbers))

(in-package #:stump-config.keys)

;; ---------------------------------------------------------------
;;; Custom Groups
;; ---------------------------------------------------------------
(defvar *refresh-map* (make-sparse-keymap)
  "talks about refreshing screen or windows")

;; ---------------------------------------------------------------
;;; General Definition Facilities
;; ---------------------------------------------------------------

(defun format-command (command)
  "we need to format the command, if it's an application format it
like a string, otherwise just leave it be"
  (if (listp command)
      (format nil "~(~a~)~{ ~a~}" (car command) (cdr command))
      command))

(defmacro defun-key (map key command)
  "Add a keybinding mapping for the key, @var{key}, to the command,
@var{command}, in the specified keymap. If @var{command} is nil, remove an
existing binding.  Unlike define-key, defnu-key takes a lisp expression"
  `(define-key ,map ,key
     (format-command
      ,(if (listp command)
           ;; if this is the case we want the car to be frozen, but we
           ;; want the arguments to be evaled
           `(list ',(car command) ,@(cdr command))
           `',command))))

(defmacro defun-key-range (map key-fun command &optional (range '(list:range 1 9)))
  "Acts like defun-key over the keyboard range of 0-9. the key-fun is
a function which takes the number, while command automatically gets
fed the argument."
  (let ((number (gensym)))
    `(progn
       (mapcar (lambda (,number)
                 (defun-key ,map (funcall ,key-fun ,number)
                   ;; have to simulate function application,
                   ;; it's awkward due to the interface stump takes
                   ;; also this is the main reason this is a macro
                   ,(if (listp command)
                        `(,@command ,number)
                        `(,command ,number))))
             ,range))))


(defun kbd-modifier-prefix (prefix)
  "sets up a kbd binding. Namely this binding includes the modifier
prefix along with concatenating the prefix to the curried number left
to send in."
  (lambda (num)
    (kbd (modifier (format nil "~a~a" prefix num)))))

;; ---------------------------------------------------------------
;;; Special commands
;; ---------------------------------------------------------------

(defcommand repack-frame-numbers () ()
  (stumpwm::group-repack-frame-numbers (current-group)))

(defcommand frame-switch-by-number (number) ((:number "Select: "))
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

(defun-key *top-map* (kbd (modifier "RET"))
  (exec "urxvt"))

(defun-key *root-map* (kbd "C-z") (echo "Z..."))

;; Repacking
(defun-key *root-map* (kbd "C-f") (repack-frame-numbers))
(defun-key *root-map* (kbd "C-w") (repack-window-numbers))

;; ---------------------------------------------------------------
;; Grouping-keys
;; ---------------------------------------------------------------

(defun-key *top-map* (kbd (modifier "g")) *groups-map*)

;; ---------------------------------------------------------------
;; Group numbering
;; ---------------------------------------------------------------

(defun-key *groups-map* (kbd "a") (stump-config.groups:grenumber))
(defun-key *groups-map* (kbd "z") (stump-config.groups:grenumber-other))

;; ---------------------------------------------------------------
;; Number selectors
;; ---------------------------------------------------------------

;; Window selection
;; Not sure I want these as they are on the root map easily
;; (defun-key-range *top-map* (kbd-modifier-prefix "") select-window-by-number)

(defun-key *top-map* (kbd (modifier "`"))
  (frame-switch-by-number 0))

(defun-key *top-map* (kbd (modifier "="))
  (frame-switch-by-number 0))

(defun-key-range *top-map* (kbd-modifier-prefix "")
  frame-switch-by-number)

(defun-key-range *top-map* (kbd-modifier-prefix "F")
    gselect
    (list:range 1 12))

;; rebind because 10 is really 0 for some reaosn
(defun-key *top-map* (kbd (modifier "F10"))
  (gselect 0))

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

;; Listing functions
(defun-key *root-map* (kbd "W") (windowlist))
(defun-key *root-map* (kbd "E") (frame-windowlist))
(defun-key *root-map* (kbd "e") (frame-windows))

(defun-key *top-map* (kbd (modifier "ESC")) (grouplist))

;; ---------------------------------
;; Volume
;; ---------------------------------

(defun-key *top-map* (kbd "XF86MonBrightnessUp")
  (exec "brightnessctl s 5%+"))

(defun-key *top-map* (kbd "XF86MonBrightnessDown")
  (exec "brightnessctl s 5%-"))

(defun-key *top-map* (kbd "XF86AudioMute")
  (exec "pactl set-sink-mute @DEFAULT_SINK@ toggle"))

(defun-key *top-map* (kbd "XF86AudioLowerVolume")
  (exec "pactl set-sink-volume @DEFAULT_SINK@ '-5%'"))

(defun-key *top-map* (kbd "XF86AudioRaiseVolume")
  (exec "pactl set-sink-volume @DEFAULT_SINK@ '+5%'"))

;; ---------------------------------
;; Uploading
;; ---------------------------------

(defun-key *root-map* (kbd "u")
  (exec "~/scripts/maim/current.sh"))

(defun-key *root-map* (kbd "U")
  (exec "~/scripts/maim/section.sh"))

;; Placing
(defun-key *root-map* (kbd "C-p") (place-existing-windows))

;; killing
(defun-key *root-map* (kbd "q") (delete))
(defun-key *root-map* (kbd "S-q") (kill))
(defun-key *root-map* (kbd "C-M-q") (quit-confirm))

;; Calling menus
(defun-key *top-map* (kbd (modifier "d")) (exec))
(defun-key *top-map* (kbd (modifier "n")) (exec "passmenu"))

;; ---------------------------------
;; Refreshing
;; ---------------------------------

;; refreshing screen size
(defun-key *root-map* (kbd "R") (refresh-heads))

(defun-key *top-map* (kbd (modifier "r")) *refresh-map*)

(defun-key *refresh-map* (kbd "r") (refresh-heads))

(defun-key *refresh-map* (kbd "w") (refresh))

(defun-key *refresh-map* (kbd "t") (refresh-time-zone))


;; ---------------------------------
;; Help functionality
;; ---------------------------------

(defcommand help-top () ()
  (stumpwm::display-bindings-for-keymaps nil *top-map*))

(defun-key *top-map* (kbd (modifier "?"))
  (help-top))
