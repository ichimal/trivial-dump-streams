(cl:in-package :cl-user)

(defpackage #:trivial-dump-streams-asd
  (:use :cl :asdf) )

(in-package #:trivial-dump-streams-asd)

(defsystem trivial-dump-streams
  :name "trivial dump streams"
  :version "0.0.1"
  :maintainer "SUZUKI Shingo"
  :author "SUZUKI Shingo"
  :licence "MIT"
  :description "converts dump string and binary mutually"
  :depends-on (:trivial-gray-streams)
  :components
    ((:module :src
      :serial t
      :components ((:file :packages)
                   (:file :conditions)
                   (:file :context)
                   (:file :filter)
                   (:file :converter)
                   (:file :stream) ))))

