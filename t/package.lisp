(uiop:define-package #:cl-scheme/test
  (:use #:cl #:rove #:cl-scheme)
  (:import-from #:cl-scheme
                #:*global-env*
                #:evaluate))
(in-package #:cl-scheme/test)

(defvar *global-test-env* (cl-scheme::create-global-env :alist-env))

(cl-scheme::env-define! *global-test-env* 'evaluate #'evaluate)
(cl-scheme::load-scheme-files *global-test-env* :quiet t)
