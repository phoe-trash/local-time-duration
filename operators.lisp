(in-package :time-span)

(declaim (inline %duration+))
(defun %duration+ (operator function first-duration durations)
  (multiple-value-bind (total-month total-nanosecond)
      (apply #'denormalize (multiple-value-list (decode-duration first-duration)))
    (dolist (duration durations)
      (multiple-value-bind (month nanosecond)
          (apply #'denormalize (multiple-value-list (decode-duration duration)))
        (setf total-month (funcall function total-month month)
              total-nanosecond (funcall function total-nanosecond nanosecond))))
    (check-sign (signum total-month) (signum total-nanosecond)
                operator (list first-duration durations))
    (duration :month total-month :nanosecond total-nanosecond)))

(defun duration+ (&rest durations)
  "Returns a fresh duration representing the sum of the lengths of its
arguments."
  (%duration+ 'duration+ #'+ (duration) durations))

(defun duration- (duration &rest durations)
  "Returns a fresh duration representing the result of subtracting the length of
each argument in turn."
  (%duration+ 'duration- #'- duration durations))

(defun duration* (duration factor &optional (rounder #'round))
  "Returns a fresh duration as long as `DURATION` multiplied by `FACTOR`."
  (multiple-value-bind (month nanosecond)
      (apply #'denormalize (multiple-value-list (decode-duration duration)))
    (duration :month (funcall rounder (* month factor))
              :nanosecond (funcall rounder (* nanosecond factor)))))

(macrolet
    ((define-variant (symbol)
       (let ((name (a:symbolicate '#:duration- symbol)))
         `(defun ,name (duration divisor)
            ,(format nil "Returns a fresh duration that is as long as ~
                          `DURATION` divided by `DIVISOR`~%via `~A`." symbol)
            (multiple-value-bind (month nanosecond)
                (apply #'denormalize (multiple-value-list (decode-duration duration)))
              (duration :month (,symbol month divisor)
                        :nanosecond (,symbol nanosecond divisor)))))))
  (define-variant floor)
  (define-variant ceiling)
  (define-variant truncate)
  (define-variant round))

(defun duration-minimum (duration &rest durations)
  (a:extremum (cons duration durations) #'duration<))

(defun duration-maximum (duration &rest durations)
  (a:extremum (cons duration durations) #'duration>))
