(ns sgfc.core
  (:gen-class))

(def file-name "resources/test.sgf")
(def file-text (slurp file-name))

(def EBNF
  {:tCollection '(:tGameTree :repetition :tGameTree)
   :tGameTree '("(" :tSequence :repetition :tGameTree ")")
   :tSequence '(:tNode :repetition :tNode)
   :tNode '(";" :repetition :tProperty)
   :tProperty '(:tPropertyIdent :tPropertyValue
                                       :repetition :tPropertyValue)
   :tPropertyIdent '(:tUcLetter :repetition :tUcLetter)
   :tPropertyValue '("[" :tCValueType "]")
   :tCValueType '(:or '(:tValueType :tCompose)
   :tValueType '(:or '(:tNone :tNumber :tReal :tDouble
                              :tColor :tSimpleText :tText
                              :tPoint :tMove :tStone))
   ;; "A" - "Z"
   :tUcLetter (map char (range (int \A) (+ (int \Z) 1)))
   ;; "0" - "9"
   :tDigit (map #(. String valueOf %) (range 0 10))
   :tNone '(""))
   :tNumber '(:or '("+" "-") :tDigit :repetition :tDigit)
   :tReal '(:tNumber :optional
                     '("." :tDigit :repetition :tDigit))
   :tDouble '(:or '("1" "2"))
   :tColor '(:or '("B" "W"))
   :tSimpleText '(:repetition :any-char)
   :tText '(:repetition :any-char)
   :tPoint '(:todo :implement)
   :tMove '(:todo :implement)
   :tStone '(:todo :implement)
   :tCompose '(:tValueType ":" :tValueType)})

(defn inside-circle? [x y]
  (>= 1 (+ (* x x) (* y y))))

(defn test-random-pair []
  (inside-circle? (rand) (rand)))

(defn PI [accuracy]
  (* 4 (/
         (count (filter true?
                        (repeatedly accuracy test-random-pair)))
         accuracy)))

