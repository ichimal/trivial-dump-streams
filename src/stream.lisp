(cl:in-package #:trivial-dump-streams)

(defclass dump-stream (fundamental-binary-input-stream
                       fundamental-binary-output-stream
                       fundamental-character-input-stream
                       fundamental-character-output-stream )
  ((context :initarg :context :initform (error "required :context"))
   (outbound-buffer :initform nil)
   (outbound-buffer-length :initform 0) ))

(defun make-dump-stream (from to &key inbound-bits outbound-bits )
  (make-instance 'dump-stream
    :context (make-dump-stream-context
               :from from
               :to to
               :inbound-bits inbound-bits
               :outbound-bits outbound-bits )))

(defmethod stream-read-byte-lookahead ((stream dump-stream))
  (with-slots (context outbound-buffer outbound-buffer-length) stream
    (let ((outbound-bits (dump-stream-context-outbound-bits context)))
      (and outbound-buffer (>= outbound-buffer-length outbound-bits)) )))

(defmethod stream-read-byte-no-hang ((stream dump-stream))
  (with-slots (context outbound-buffer outbound-buffer-length) stream
    (let ((outbound-bits (dump-stream-context-outbound-bits context)))
      (let ((value (ldb (byte outbound-bits 0) outbound-buffer)))
        (decf outbound-buffer-length outbound-bits)
        (if (zerop outbound-buffer-length)
          (setf outbound-buffer nil)
          (setf outbound-buffer (ash outbound-buffer (- outbound-bits))) )
        value ))))

(defmethod stream-read-byte ((stream dump-stream))
  (loop until (stream-read-byte-lookahead stream)
        do (sleep 0.01) )
  (stream-read-byte-no-hang stream) )

(defmethod stream-write-char ((stream dump-stream) (c character))
  (unless (dump-stream-ignoring-char-p stream c)
    (dump-stream-write-char stream c)))

(defun dump-stream-write-char (stream c)
  (declare (type stream dump-stream) (type character c))
  (with-slots (context outbound-buffer outbound-buffer-length) stream
    (multiple-value-bind (value length)
        (ecase (dump-stream-context-from context)
          (:hexadecimal (hex-char-to-nibble c))
          (:decimal (dec-char-to-nibble c))
          (:octal (oct-char-to-3-bits c))
          (:binary
            (error (make-condition 'invalid-char
                                   :character c
                                   :accepted-character-class :binary ))))
      (if (null outbound-buffer)
        (setf outbound-buffer value
              outbound-buffer-length length )
        (psetf outbound-buffer (dpb value
                                   (byte length outbound-buffer-length)
                                   outbound-buffer )
               outbound-buffer-length (+ outbound-buffer-length length) )))))

