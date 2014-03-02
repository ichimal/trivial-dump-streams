(cl:in-package #:trivial-dump-streams)

(defun dump-stream-ignoring-char-p (stream c)
  (declare (type stream dump-stream) (type character c))
  (or (dump-stream-delimitier-char-p stream c)
      (dump-stream-skipping-char-p stream c) ))

(defun dump-stream-delimitier-char-p (stream c)
  (declare (type stream dump-stream) (type character c))
  ;; just stub
  ;; it should be user selectable
  (not (null (position c #(#\space #\tab #\return #\linefeed #\page)))) )

(defun dump-stream-skipping-char-p (stream c)
  (declare (type stream dump-stream) (type character c))
  ;; just stub
  nil )

