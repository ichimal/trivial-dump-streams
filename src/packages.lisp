(cl:in-package #:trivial-dump-streams-asd)

(defpackage #:trivial-dump-streams
  (:use :cl :trivial-gray-streams)
  (:nicknames :tds)
  (:export #:dump-stream
           #:make-dump-stream
           #:config-dump-stream ))

