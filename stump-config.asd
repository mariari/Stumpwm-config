;; We use my personal stdlib which should be loaded in the .sbclrc

;; (load "/home/katyusha/Documents/Workspace/Lisp/Misc-Lisp-Scripts/misc.asd")
;; (asdf:load-system :misc)

(asdf:defsystem stump-config
  :depends-on (:stumpwm :misc)
  :version "0.0.0.1"
  :maintainer "mariari"
  :author "mariari"
  :description "Stumpwm configuration"
  :components ((:file "src/groups")
               (:file "src/keys" :depends-on ("src/group"))))

(asdf:defsystem stump-config/startup
  :depends-on (:stumpwm :misc :stump-config)
  :version "0.0.0.1"
  :maintainer "mariari"
  :author "mariari"
  :description "Stumpwm configuration startup"
  :components ((:file "src/startup")))
