(in-package :cl-user)
(defpackage #:cl-sqids
  (:use
    #:common-lisp)
  (:export
    #:make-sqids
    #:decode
    #:encode))
