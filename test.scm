;;;
;;; Test paradoc
;;;

(use gauche.test)

(test-start "scribble")
(use scribble)
(test-module 'scribble)


;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




