(in-package :time-span-tests)

(5am:def-suite time-span-tests)
(5am:in-suite time-span-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Duration tests

(5am:test duration-equality
  (5am:is (ts:duration= (ts:duration :day 1) (ts:duration :hour 24))))

(5am:test duration-comparison
  (5am:is (ts:duration< (ts:duration :day 1) (ts:duration :hour 48)))
  (5am:is (ts:duration> (ts:duration :day 1) (ts:duration :hour 12))))

(5am:test duration-in-units
  (5am:is (eql (ts:duration-as (ts:duration :day 1) :second) 86400)))

(5am:test duration-sum
  (5am:is (ts:duration= (ts:duration+ (ts:duration :day 1)
                                      (ts:duration :hour 12)
                                      (ts:duration :minute 30))
                        (ts:duration :second 131400))))

(5am:test duration-difference
  (5am:is (ts:duration= (ts:duration- (ts:duration :day 2)
                                      (ts:duration :day 1)
                                      (ts:duration :hour 12))
                        (ts:duration :second 43200))))

(5am:test duration-multiply
  (5am:is (ts:duration= (ts:duration* (ts:duration :day 1) 2)
                        (ts:duration :day 2))))

(5am:test duration-divide
  ;; TODO all other operators
  (5am:is (ts:duration= (ts:duration-truncate (ts:duration :day 2) 2)
                        (ts:duration :day 1))))

(5am:test duration-minimum
  (5am:is (ts:duration= (ts:duration-minimum (ts:duration :day 2)
                                             (ts:duration :hour 36)
                                             (ts:duration :day 4))
                        (ts:duration :hour 36))))

(defun gen-timestamp ()
  (lambda ()
    (flet ((rand-in-range (range-size)
             (- (random range-size) (/ range-size 2))))
      (lt:adjust-timestamp (lt:now)
        (:offset :year (rand-in-range 40))
        (:offset :month (rand-in-range 24))
        (:offset :day (rand-in-range 180))
        (:offset :minute (rand-in-range 600))
        (:offset :sec (rand-in-range 3600))
        (:offset :nsec (rand-in-range (expt 10 9)))))))

(5am:test duration-associates
  "Test that, for any pair of timestamps, this always holds:

  (+ b (difference a b)) == a"
  (let ((lt:*default-timezone* lt:+utc-zone+))
    (5am:for-all ((a (gen-timestamp))
                  (b (gen-timestamp)))
      (5am:is (lt:timestamp=
               a (ts:add-duration b (ts:timestamp-difference a b)))))))

(5am:test timestamp-difference
  (5am:is (ts:duration= (ts:timestamp-difference (lt:parse-timestring "2014-01-01T09:00:00")
                                                 (lt:parse-timestring "2014-01-01T04:30:00"))
                        (ts:duration :hour 4 :minute 30))))

(5am:test timestamp-add-duration
  (5am:is (lt:timestamp= (ts:add-duration (lt:parse-timestring "2014-01-01T09:00:00")
                                          (ts:duration :hour 3))
                         @2014-01-01T12:00:00)))

(5am:test timestamp-subtract-duration
  (5am:is (lt:timestamp= (ts:subtract-duration (lt:parse-timestring "2014-01-01T09:00:00")
                                               (ts:duration :hour 3))
                         @2014-01-01T06:00:00)))
