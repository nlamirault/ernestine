;;
;; Continuous integration: launch unit tests
;;

(in-package :cl-user)

(load ".clenv/.quicklisp/setup.lisp")

(ql:quickload "ernestine")
(ql:quickload "ernestine-tests")

(setq lisp-unit:*print-failures* t)
(setq lisp-unit:*print-errors* t)
(setq lisp-unit:*print-summary* t)

(ernestine-tests:run-ernestine-tests)

