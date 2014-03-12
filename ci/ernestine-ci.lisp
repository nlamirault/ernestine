;;
;; Continuous integration: launch unit tests
;;

(in-package :cl-user)

(load ".clenv/.quicklisp/setup.lisp")

(ql:quickload "ernestine")
(ql:quickload "ernestine-tests")

(ernestine-tests:run-ernestine-tests)

