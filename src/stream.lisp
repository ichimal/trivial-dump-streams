(cl:in-package #:trivial-dump-streams)

(defclass dump-stream ()
  ((context :initarg :context :initform (error "required :context"))
   (outbound-buffer :initform (make-queue :simple-queue))
   (temporary-buffer :initform 0)
   (temporary-buffer-length :initform 0) ))

(defclass from-character-dump-stream (fundamental-character-output-stream)
  () )

(defclass from-binary-dump-stream (fundamental-binary-output-stream)
  () )

(defclass to-character-dump-stream (fundamental-character-input-stream)
  () )

(defclass to-binary-dump-stream (fundamental-binary-input-stream)
  () )

(defclass character-to-binary-dump-stream
  (dump-stream from-character-dump-stream to-character-dump-stream)
  () )

(defclass binary-to-character-dump-stream
  (dump-stream from-binary-dump-stream to-character-dump-stream)
  () )

(defclass character-to-character-dump-stream
  (dump-stream from-character-dump-stream to-character-dump-stream)
  () )

(defclass binary-to-binary-dump-stream
  (dump-stream from-binary-dump-stream to-binary-dump-stream)
  () )

(defmethod initialize-instance :after ((obj dump-stream)
                                       &key &allow-other-keys )
  (let ((target (with-slots (context) obj
                  (with-slots (from to) context
                    (if (eq from :binary)
                      (if (eq to :binary)
                        'binary-to-binary-dump-stream
                        'binary-to-character-dump-stream )
                      (if (eq to :binary)
                        'character-to-binary-dump-stream
                        'character-to-character-dump-stream ))))))
    (change-class obj target)))

(defun make-dump-stream (from to &key inbound-bits outbound-bits )
  (make-instance 'dump-stream
    :context (make-dump-stream-context
               :from from
               :to to
               :inbound-bits inbound-bits
               :outbound-bits outbound-bits )))

#+clisp
(defmethod gray:stream-read-byte-lookahead ((stream to-binary-dump-stream))
  (with-slots (outbound-buffer) stream
    (plusp (qsize outbound-buffer)) ))

#+clisp
(defmethod gray:stream-read-byte-no-hang ((stream to-binary-dump-stream))
  (with-slots (outbound-buffer) stream
    (qpop outbound-buffer) ))

(defmethod stream-read-byte ((stream to-binary-dump-stream))
  (loop until (stream-read-byte-lookahead stream)
        do (sleep 0.001) )
  (stream-read-byte-no-hang stream) )

(defmethod stream-read-char-will-hang-p ((stream to-character-dump-stream))
  (with-slots (outbound-buffer) stream
    (plusp (qsize outbound-buffer)) ))

(defmethod stream-read-char-no-hang ((stream to-character-dump-stream))
  (with-slots (outbound-buffer) stream
    ;; stub
    (format nil "~x" (qpop outbound-buffer)) ))

(defmethod stream-read-char ((stream to-character-dump-stream))
  (loop until (stream-read-char-will-hang-p stream)
        do (sleep 0.001) )
  (stream-read-char-no-hang stream) )

(defmethod stream-write-char ((stream from-character-dump-stream) (c character))
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
      (update-internal-buffer stream value length) )))

(defun update-internal-buffer (stream value length)
  (declare (type dump-stream stream)
           (type unsigned-byte value length) )
  (with-slots (context outbound-buffer
               temporary-buffer temporary-buffer-length )
              stream
    ;; phase 1: merge temporary buffer
    (let* ((total-length (+ temporary-buffer-length length))
           (merged-buffer
             (logand (dpb temporary-buffer
                          (byte temporary-buffer-length length)
                          value )
                     (1- (ash 1 total-length)) ))
           (unit-length (dump-stream-context-outbound-bits context)) )
      ;; phase 2: split merged buffer into the outbound queue
      (loop with current-length = total-length
            while (>= current-length unit-length)
            do (decf current-length unit-length)
               (qpush outbound-buffer (ash merged-buffer (- current-length)))
               (setf merged-buffer
                     (logand merged-buffer (1- (ash 1 current-length))))
            finally (setf temporary-buffer-length current-length
                          temporary-buffer merged-buffer) ))))

