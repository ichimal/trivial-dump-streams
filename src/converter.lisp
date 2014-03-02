(cl:in-package #:trivial-dump-streams)

(defparameter +hex-table+ "0123456789ABCDEF")
(defparameter +dec-table+ "0123456789")
(defparameter +oct-table+ "01234567")

(declaim (ftype (function (character) (values (unsigned-byte 4) (unsigned-byte 8)))
                hex-char-to-nibble ))
(defun hex-char-to-nibble (c)
  (declare (type character c))
  (let ((value (position (char-upcase c) +hex-table+)))
    (unless value
      (error (make-condition 'invalid-character
                             :character c
                             :accepted-character-class :hexadecimal )))
    (values value 4) ))

(declaim (ftype (function (character) (values (unsigned-byte 4) (unsigned-byte 8)))
                dec-char-to-nibble ))
(defun dec-char-to-nibble (c)
  (declare (type character c))
  (let ((value (position (char-upcase c) +dec-table+)))
    (unless value
      (error (make-condition 'invalid-character
                             :character c
                             :accepted-character-class :decimal )))
    (values value 4) ))

(declaim (ftype (function (character) (values (unsigned-byte 3) (unsigned-byte 8)))
                oct-char-to-3-bits ))
(defun oct-char-to-3-bits (c)
  (declare (type character c))
  (let ((value (position (char-upcase c) +oct-table+)))
    (unless value
      (error (make-condition 'invalid-character
                             :character c
                             :accepted-character-class :octal )))
    (values value 3) ))

