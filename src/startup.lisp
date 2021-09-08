(defpackage #:stump-config.startup
  (:documentation "The startup program that should be ran once for stumpwm")
  (:use #:common-lisp #:stumpwm)
  (:export
   ))

(in-package :stump-config.startup)

(run-shell-command "~/.fehbg")
(run-shell-command "nm-applet")
(run-shell-command "blueman-applet")

(run-shell-command "mate-volume-control-applet")
(run-shell-command "fcitx")

;; (run-shell-command
;;  "trayer --edge top --align center --SetDockType true --SetPartialStrut true \
;;  \ --expand true --width 12 --transparent true --tint 0x191970 --height 14 \
;;  \ --monitor primary")

;; (run-shell-command )
