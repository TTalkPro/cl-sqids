;;; CL-SQIDS system definition
;;; (c) David Gao.

(in-package :asdf)

(defsystem #:cl-sqids
  :version "1.0.0"
  :author "David Gao <david.alpha.fox@gmail.com>"
  :maintainer "David Gao <david.alpha.fox@gmail.com>"
  :license "Apache 2.0"
  :homepage "http://www.ttalk.im/projects/cl-sqids"
  :bug-tracker "https://github.com/TTalkPro/cl-sqids/issues"
  :source-control (:git "https://github.com/TTalkPro/cl-sqids.git")
  :description "A Common Lisp library for sqids id"
  :components ((:module "src"
                 :serial t
                 :components
                 ((:file "package")
                   (:file "constants")
                   (:file "conditions")
                   (:file "core")
                   (:file "cl-sqids")))))
