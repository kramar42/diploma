(ns sgfc.game
  (:import
   [java.math BigInteger]
   [java.util BitSet
    Random]))


(def uc-board-letters (remove #{\I} (sgfc.tools/gen-letters \A \T)))
(def board-letters (->> uc-board-letters (map clojure.string/lower-case) set))
(def numbering (zipmap board-letters (range 1 20)))


;; Pretty printing
(defn- print-line [coll]
  (println (apply str (flatten (map list coll (repeat \ ))))))


(defn print-board [board]
  (print "   ")
  (print-line uc-board-letters)
  (doall (map (fn [line number]
                (print number)
                (if (< number 10)
                  (print "  ")
                  (print " "))
                (print-line
                 (map (fn [elem]
                        (condp = elem
                          :black \X
                          :white \O
                          :empty \.))
                      line)))
              board (range 1 20))))


;; Additional functions
(defn- mkplayer []
  (let [player (atom false)]
    (fn [] (swap! player not))))


(defn- char2index [char]
  (numbering char))


(defn- index2char [idx]
  (char (+ idx (int \a))))


(defn- stone [player]
  (if player
    :black
    :white))


(defn- opposite [player]
  (case player
    :black :white
    :white :black))


;; MINIMAX
(defn- heuristic [board player]
  (- (rand-int 100) 50))


(defn- make-move [board move player]
  (let [x (char2index (first move))
        y (char2index (second move))]
    (assoc board x (assoc (board x) y player))))


(defn- movegen [board player]
  (take 10 (shuffle
  (filter (complement nil?)
          (flatten
            (map (fn [[row x]]
                   (map (fn [[element y]]
                          (if (= element :empty)
                            (str x y)))
                        (map (partial list) row board-letters)))
                 (map (partial list) board board-letters)))))))


(defn- minimax [board depth player firstrun]
  ;(println (str "minimax " depth " " player))
  (let [moves (movegen board player)]
    (if (or (empty? moves) (zero? depth))
      [(heuristic board player) nil]
      (do
        ;(println "Processing moves...")
        (apply max-key first
               (cons
                 [-9999 nil]
                 ((if firstrun pmap map) (fn [move]
                        ;(println "---> " move)
                        (let [[value path]
                              (minimax (make-move board move player) (dec depth)
                                       (opposite player) false)]
                          ;(println "<--- " value)
                          [(- value) (cons move path)]))
                      moves)))))))


(defn- make-moves [board moves player]
  (reduce (partial apply make-move)
            board
            (map list
                 moves
                 (flatten (repeat (/ (count moves) 2) [player (opposite player)])))))


;; Zobrist
(def hash-size 128)
  (defn random-bitset [rng size]
    (let [bigint (BigInteger. size rng)
          bitset (BitSet. size)]
      (dotimes [i size]
        (.set bitset i (.testBit bigint i)))
      bitset))

  (let [rng (Random.)
        black-hashes (into-array BitSet (repeatedly 361 #(random-bitset rng hash-size)))
        white-hashes (into-array BitSet (repeatedly 361 #(random-bitset rng hash-size)))]
    (defn move-hash [pos color]
      (case color
        :black (aget black-hashes pos)
        :white (aget white-hashes pos))))


(defn toggle [board-hash pos color]
  (let [pos (+ (* 19 (char2index (first pos))) (char2index (second pos)))]
    (.xor board-hash (move-hash pos color))))


(defn zobrist [board-hash moves player]
  (map (fn [[move color]] (toggle board-hash move color))
       (map list moves
            (flatten (repeat (/ (count moves) 2) [player (opposite player)]))))
  nil)


;; Convert tree to board form
(defn tree2board [tree]
  (let [board (vec (repeatedly 19 (fn [] (vec (repeat 19 :empty)))))
        player (mkplayer)]
    (reduce (fn [board move]
              (let [y (char2index (first move))
                    x (char2index (second move))]
                (assoc board x (assoc (board x) y (stone (player))))))
            board tree)))
