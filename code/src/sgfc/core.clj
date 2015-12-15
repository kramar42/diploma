(ns sgfc.core
  (:gen-class)
  (:import
    [java.math
     BigInteger]
    [java.util
     BitSet
     Random])
  (:require [clojure.string :as s]))

;; Predefined lists
(defn- gen-letters [start end]
  ((comp set doall map) char (range (int start) (+ 1 (int end)))))

(def uc-letters (gen-letters \A \Z))
(def uc-board-letters (remove #{\I} (gen-letters \A \T)))
(def board-letters ((comp set s/lower-case s/join) uc-board-letters))
(def numbering (zipmap board-letters (range 1 20)))

;; Working with SGF-files
(defn lazy-read [file len]
  (let [rdr (clojure.java.io/reader file)
        is-closed? (atom false)
        array (char-array len)]
    (fn next-char []
      (if @is-closed?
        nil
        (let [result (.read rdr array)]
          (if (= -1 result)
            (do (.close rdr)
                (swap! is-closed? complement)
                nil)
            array))))))

(defn read-char [lazy-read-fn *stack]
  (if-let [result (first @*stack)]
    (do
      (swap! *stack rest)
      result)
    (do
      (let [string (lazy-read-fn)
            result (first string)]
        (swap! *stack concat (rest string))
        result))))

(defn unread-char [chr *stack]
  (swap! *stack (partial cons chr))
  chr)


(defn append [col1 col2]
  (into col2 (reverse col1)))

(defn lexer
  ([input-chars-fn *stack]
   (lexer input-chars-fn *stack nil))

  ([input-chars-fn *stack result]
   (letfn [
           ;; first helper function
           ;; parse input file into tree structure
           (process-chars [input-chars-fn *stack result]
             (if-let [next-char (read-char input-chars-fn *stack)]
               (condp = next-char
                 \( :treestart
                 \) :treeend
                 \; :nodestart
                 \] :propvalueend
                 ;; process SGF property value
                 \[ (list :propvalue
                          (apply str (process-prop-value input-chars-fn *stack)))
                 ;; process SFG property
                 (if (contains? uc-letters next-char)
                   (list :propident
                         (apply str (cons next-char
                                          (process-prop-ident input-chars-fn *stack))))
                   (recur input-chars-fn *stack result)))
               nil))

           ;; parse property
           (process-prop-ident [input-chars-fn *stack]
             (if-let [next-char (read-char input-chars-fn *stack)]
               (if (some #{next-char} uc-letters)
                 (cons next-char (process-prop-ident input-chars-fn *stack))
                 (do (unread-char next-char *stack) nil))))

           (process-prop-value [input-chars-fn *stack]
             (if-let [next-char (read-char input-chars-fn *stack)]
               (condp = next-char
                 \\ (let [nnext-char (read-char input-chars-fn *stack)]
                      (into () (list nnext-char next-char)
                            (process-prop-value input-chars-fn *stack)))
                 \] (do (unread-char next-char *stack) nil)
                 (cons next-char (process-prop-value input-chars-fn *stack)))))]

     (if-let [next-token (process-chars input-chars-fn *stack result)]
       (recur input-chars-fn *stack (cons next-token result))
       result))))

(defn nilcons [tree subtree]
  (if (nil? tree)
    subtree
    (cons tree subtree)))

(defn append [col1 col2]
  (into col2 (reverse col1)))

(defn parser [lexems]
  (letfn [(process-file [lexems tree]
            ;(println "process-files: " (first lexems))
            (let [lexem (first lexems)
                  lexems (rest lexems)]
              (condp = lexem
                :treestart (let [[lexems subtree] (process-tree lexems nil)
                                 subtree (reverse subtree)]
                             (recur lexems (cons subtree tree)))
                :nodestart (let [[lexems subtree] (process-tree (cons lexem lexems) nil)
                                 subtree (reverse subtree)]
                             (recur lexems (cons subtree tree)))
                :treeend (recur lexems tree)
                (reverse tree))))

          (process-tree [lexems tree]
            ;(println "process-tree: " (first lexems))
            (let [lexem (first lexems)
                  lexems (rest lexems)]
              (condp = lexem
                :nodestart (let [[lexems subtree] (process-nodes lexems nil)
                                 subtree (reverse subtree)]
                             (recur lexems (cons subtree tree)))
                (list (cons lexem lexems) tree))))

          (process-nodes [lexems tree]
            ;(println "process-nodes: " (first lexems))
            (if (and
                  (instance? clojure.lang.PersistentList (first lexems))
                  (= (ffirst lexems) :propident))
              (let [propident (second (first lexems))
                    [lexems subtree] (process-values (rest lexems) nil)
                    subtree (reverse subtree)]
                (recur lexems (cons (cons propident subtree) tree)))
              (list lexems tree)))

          (process-values [lexems tree]
            ;(println "process-values: " (first lexems))
            (if (and
                  (instance? clojure.lang.PersistentList (first lexems))
                  (= (ffirst lexems) :propvalue)
                  (= (second lexems) :propvalueend))
              (recur (nnext lexems) (cons (second (first lexems)) tree))
              (list lexems tree)))]

    (process-file lexems nil)))

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

;; Pretty printing
(defn- print-line [coll]
  (println (apply str (flatten (map list coll (repeat \ ))))))

(defn print-board [board]
  (do
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
  nil)

(defn sgf2lexems [file-name]
  (let [lazy-read-fn (lazy-read file-name 512)
        *stack (atom nil)
        lexems (reverse (lexer lazy-read-fn *stack nil))]
    lexems))

(defn sgf2tree [file-name]
  ((comp parser sgf2lexems) file-name))

;; Simplifying functions
(defn simplify [tree]
  (if (instance? java.lang.String (first tree))
    ;; process property ident/value pair
    (let [color (first tree)
          move (second tree)]
      (if (or
            (= color "W")
            (= color "B"))
        move))
    (filter (complement (and nil? empty?)) (pmap simplify tree))))

;; Convert tree to board form
(defn tree2board [tree]
  (let [board (vec (repeatedly 19 (fn [] (vec (repeat 19 :empty)))))
        player (mkplayer)]
    (reduce (fn [board move]
              (let [y (char2index (first move))
                    x (char2index (second move))]
                (assoc board x (assoc (board x) y (stone (player))))))
            board tree)))

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

;; MAIN
(defn -main [& args]
  (minimax (tree2board (flatten (simplify (sgf2tree (first args)))))
           (second args) :black))
