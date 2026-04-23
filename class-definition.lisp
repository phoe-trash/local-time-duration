(in-package :time-span)

(protest/base:define-protocol-class duration ()
  ((day :accessor duration-day :initarg :day
        :initform 0 :type integer)
   (hour :accessor duration-hour :initarg :hour
         :initform 0 :type integer)
   (minute :accessor duration-minute :initarg :minute
           :initform 0 :type integer)
   (second :accessor duration-second :initarg :second
           :initform 0 :type integer)
   (nanosecond :accessor duration-nanosecond :initarg :nanosecond
               :initform 0 :type integer)
   (sign :accessor duration-sign :initarg :sign
         :initform 1 :type (member 1 0 -1)))
  (:documentation "A duration protocol class. Do not instantiate directly."))

(defclass year-month-duration (duration)
  ;; These slots are not allowed to be zero at the same time.
  ((year :accessor duration-year :initarg :year
         :initform 0 :type integer)
   (month :accessor duration-month :initarg :month
          :initform 0 :type integer))
  (:documentation
   "An ISO 8601-2 compatible duration containing month and year elements."))

(defclass week-duration (duration)
  ((week :accessor duration-week :initarg :week
         :initform 0 :type integer))
  (:documentation
   "An ISO 8601-2 compatible duration containing a week element."))
