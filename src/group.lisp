(defpackage #:stump-config.groups
  (:documentation "Provides extra group operations")
  (:use #:common-lisp #:stumpwm)
  (:export :grenumber :grenumber-other))

(in-package :stump-config.groups)


(defun swap-group-numbers (group-1 group-2)
  "swaps two groups numbers"
  (let ((group-1-num (group-number group-1))
        (group-2-num (group-number group-2)))
    (setf (group-number group-1) group-2-num
          (group-number group-2) group-1-num)))

(defun set-group-number (group number)
  "sets a groups number to the request one"
  (setf (group-number group) number))

(defun find-group-from-number (screen number)
  "finds the group with a particular number. Returns nil if none is found"
  ;; only 1 element can be found, so get the car
  (car
   (remove-if-not (lambda (g) (= (group-number g) number))
                  (screen-groups screen))))

(defcommand grenumber (number) ((:number "number-to-set: "))
  "renumbers the current group to the one selected by the user. If a
group exists which overlaps, then the numbering shall be swapped"
  (let ((target-group (find-group-from-number (stumpwm:current-screen) number))
        (group        (current-group)))
    (if target-group
        (swap-group-numbers target-group group)
        (set-group-number group number))))

(defcommand grenumber-other (to-start to-swap-to) ((:number "number-from: ")
                                                   (:number "number-to-set: "))
  "renumbers the current group to the one selected by the user. If a
group exists which overlaps, then the numbering shall be swapped"
  (let* ((screen       (stumpwm:current-screen))
         (start-group  (find-group-from-number screen to-start))
         (target-group (find-group-from-number screen to-swap-to)))
    (cond ((and start-group target-group)
           (swap-group-numbers start-group target-group))
          (to-start
           (set-group-number start-group to-swap-to)))))
