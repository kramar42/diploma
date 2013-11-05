(ns lab-0.core-test
  (:require [clojure.test :refer :all]
            [lab-0.core :refer :all ]))

(deftest stack-test-push
  (is ((push 1) '(1)))
  (is ((push 2) '(2 1))))


(deftest stack-test-pop
  (is ((pop) 2))
  (is ((pop) 1)))

;(deftest stack-test-push-pop
; (map '#(is ())))

(deftest stack-all-test
  (stack-test-push)
  (stack-test-pop))

(run-tests 'lab-0.core-test)
