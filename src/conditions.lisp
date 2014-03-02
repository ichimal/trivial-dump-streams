(cl:in-package #:trivial-dump-streams)

(define-condition invalid-character (stream-error)
  ((character
     :type character
     :reader invalid-character
     :initarg :character
     :initform (error "required :character") )
   (accepted-character-class
     :type symbol
     :reader accepted-character-class
     :initarg :accepted-character-class
     :initform (error "required :accepted-character-class") ))
  (:report (lambda (condition stream)
             (format stream "cannot accept '~s' for ~a stream"
                     (invalid-character condition)
                     (accepted-character-class condition) ))))

(defmethod print-object ((obj invalid-character) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s :accepted=~a"
            (invalid-character obj)
            (accepted-character-class obj) )))

