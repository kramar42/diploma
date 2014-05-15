(ns sgfc.core
  (:gen-class))

(def file-name "resources/test.sgf")

(def EBNF
  {:Collection [:GameTree :repetition :GameTree]
   :GameTree ["(" :Sequence :repetition :GameTree ")"]
   :Sequence [:Node :repetition :Node]
   :Node [";" :repetition :Property]
   :Property [:PropIdent :PropValue
              :repetition :PropValue]
   :PropIdent [:UcLetter :repetition :UcLetter]
   :PropValue ["[" :CValueType "]"]
   :CValueType [:or [:ValueType :Compose]]
   :ValueType [:or [:None :Number :Real :Double
                     :Color :SimpleText :Text
                     :Point :Move :Stone]]})

(def Type
  {
   :None [""]
   ;; "A" - "Z"
   :UcLetter (map char (range (int \A) (+ (int \Z) 1)))
   ;; "0" - "9"
   :Digit (map #(. String valueOf %) (range 0 10))

   :Number [:optional :or ["+" "-"] :Digit :repetition :Digit]
   :Real [:Number :optional ["." :Digit :repetition :Digit]]

   :Double [:or ["1" "2"]]
   :Color [:or ["B" "W"]]

   :SimpleText [:repetition :any-char]
   :Text [:repetition :any-char]

   :Point [:todo :implement]
   :Move [:todo :implement]
   :Stone [:todo :implement]

   :Compose [:ValueType ":" :ValueType]})

(defn lazy-read [file]
  (let [rdr (clojure.java.io/reader file)]
    (fn next-char []
      (lazy-seq
        (let [chr (.read rdr)]
          (if (= -1 chr)
            (do (.close rdr) nil)     
            (cons (char chr) (next-char))))))))

(def ^:dynamic *lazy-read-fn*
  (lazy-read file-name))

(defn -main [& args]

  )
