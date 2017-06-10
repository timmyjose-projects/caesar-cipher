#|
  This file is a part of caesar-cipher project.
  Copyright (c) 2017 Timmy Jose (zoltan.jose@gmail.com)
|#

#|
  Author: Timmy Jose (zoltan.jose@gmail.com)
|#

(in-package :cl-user)
(defpackage caesar-cipher-asd
  (:use :cl :asdf))
(in-package :caesar-cipher-asd)

(defsystem caesar-cipher
  :version "0.1"
  :author "Timmy Jose"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "caesar-cipher"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op caesar-cipher-test))))
