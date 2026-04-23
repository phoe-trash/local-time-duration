(in-package :time-span)

(defun timestamp-difference (time-a time-b)
  "Returns a duration representing the time elapsed between the timestamps
`TIME-A` and `TIME-B`. This duration may be negative if `TIME-B` is later than
`TIME-A`."
  (let ((day (- (local-time:day-of time-a)
                (local-time:day-of time-b)))
        (sec (- (local-time:sec-of time-a)
                (local-time:sec-of time-b)))
        (nsec (- (local-time:nsec-of time-a)
                 (local-time:nsec-of time-b))))
    (duration :day day :second sec :nanosecond nsec)))

(defun add-duration (timestamp duration)
  "Returns a fresh timestamp representing the time when `DURATION` has elapsed
after `TIMESTAMP`."
  (multiple-value-bind (month nanosecond)
      (multiple-value-call #'denormalize (decode-duration duration))
    (local-time:adjust-timestamp timestamp
      (offset :month month)
      (offset :nsec nanosecond))))

(defun subtract-duration (timestamp duration)
  "Returns a fresh timestamp representing the time when `DURATION` will elapse
before `TIMESTAMP`."
  (multiple-value-bind (month nanosecond)
      (multiple-value-call #'denormalize (decode-duration duration))
    (local-time:adjust-timestamp timestamp
      (offset :month (- month))
      (offset :nsec (- nanosecond)))))
