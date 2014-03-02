(cl:in-package #:trivial-dump-streams)

(defstruct dump-stream-context
  (from :unconfigurated :type keyword)
  (to :unconfigurated :type keyword)
  (inbound-bits nil :type (or null (integer (0) (256))))
  (outbound-bits nil :type (or null (integer (0) (256)))) )

(defmethod update-dump-stream-configuration
    ((obj dump-stream-context) from to
     &key inbound-bits outbound-bits )
  "configurate inbound and outbound behavior of the dump-stream"
  (when (or inbound-bits (not (eq from (dump-stream-context-from obj))))
    ;; setup output-stream configuration
    ;; i.e. stream's inbound configuration
    (setf (dump-stream-context-from obj) from)
    (cond (inbound-bits
            (unless (eq from :binary)
              (error "cannot specify inbound-bits for non-binary output stream") )
            (setf (dump-stream-context-inbound-bits obj) inbound-bits) )
          (t (setf (dump-stream-context-inbound-bits obj) nil)) ))

  (when (or outbound-bits (not (eq from (dump-stream-context-from obj))))
    ;; setup input-stream configuration
    ;; i.e. stream's outbound configuration
    (setf (dump-stream-context-from obj) from)
    (cond (outbound-bits
            (unless (eq from :binary)
              (error "cannot specify outbound-bits for non-binary input stream") )
            (setf (dump-stream-context-outbound-bits obj) outbound-bits) )
          (t (setf (dump-stream-context-outbound-bits obj) nil)) )))

(defmethod config-hex-to-binary-context ((obj dump-stream-context))
  (update-dump-stream-configuration obj :hexadecimal :binary :outbound-bits 4))

(defmethod config-dec-to-binary-context ((obj dump-stream-context))
  (update-dump-stream-configuration obj :decimal :binary :outbound-bits 4))

(defmethod config-oct-to-binary-context ((obj dump-stream-context))
  (update-dump-stream-configuration obj :octal :binary :outbound-bits 3))

