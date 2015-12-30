(ns sgfc.sgf
  (:require [sgfc.tools])
  (:require
   [taoensso.timbre :as timbre]
   [taoensso.timbre.profiling :as profiling
    :refer (pspy pspy* profile defnp p p*)])
  (:require [clojure.string :as s])
  (:import [clojure.lang PersistentVector]))


;; upper case letters
(def uc-letters (sgfc.tools/gen-letters \A \Z))


;; Simplifying functions
(defn get-moves [black white moves]
  (if (> (count moves) 1)
    (let [[color move & other] moves]
      (condp = color
        "B" (recur (conj black move) white other)
        "W" (recur black (conj white move) other)))
    [black white]))


(defn simplify [[[header subtree]]]
  ; also need to get starting position from header
  (->> subtree
      flatten
      (get-moves [] [])))


;; lazily read file
(defnp lazy-file-lines [filename]
  (letfn [(helper [rdr]
            (lazy-seq
             (if-let [line (.readLine rdr)]
               (p :cons-line (cons line (helper rdr)))
               (do (.close rdr) nil))))]
    (helper (clojure.java.io/reader filename))))


(defnp lazy-file-chars [filename]
  (->> filename
       lazy-file-lines
       (apply concat)
       lazy-seq))


;; read char from stack or lazy-seq
(defnp read-char [chars stack]
  (if-let [char (first @stack)]
    (do
      (swap! stack rest)
      char)
    (let [char (first @chars)]
      (swap! chars rest)
      char)))


;; unread char to stack
(defnp unread-char [char stack]
  (swap! stack #(conj % char))
  nil)


;; process property value as sequence of letters until ]
(defnp ->process-prop-value [chars stack]
  (when-let [next-char (read-char chars stack)]
    (condp = next-char
      \\ (let [nnext-char (read-char chars stack)]
           (into (list nnext-char next-char)
                 (->process-prop-value chars stack)))
      \] nil
      (conj (->process-prop-value chars stack) next-char))))


;; process property as sequence of upper case letters
(defnp ->process-prop-ident [chars stack]
  (when-let [next-char (read-char chars stack)]
    (if (some #{next-char} uc-letters)
      (conj (->process-prop-ident chars stack) next-char)
      (unread-char next-char stack))))


;; top-level lexer function
(defn ->process-chars [chars stack result]
  (when-let [next-char (read-char chars stack)]
    (condp = next-char
      \( :treestart
      \) :treeend
      \; :nodestart
      \[ (p :propvalue [:propvalue
                        (clojure.string/join (->process-prop-value chars stack))])
      (if (uc-letters next-char)
        (p :propident [:propident
                       (clojure.string/join
                        (conj (->process-prop-ident chars stack) next-char))])
        (recur chars stack result)))))


;; parse input chars seq into lexems seq
(defn ->lexer
  ([chars]
   (->lexer (atom chars) (atom nil) []))

  ([chars stack result]
   (if-let [next-token (->process-chars chars stack result)]
     (recur chars stack (conj result next-token))
     result)))


(defn process-values-> [[lexem & lexems :as lexemss] tree]
  (if (instance? PersistentVector lexem)
    (let [[keyword propvalue] lexem]
      (if (= :propvalue keyword)
        (recur lexems (conj tree propvalue))
        [lexemss tree]))
    [lexemss tree]))


(defn process-node-> [[lexem & lexems :as lexemss] tree]
  (if (instance? PersistentVector lexem)
    (let [[keyword propname] lexem]
      (when (= :propident keyword)
        (let [[lexems propvalues] (process-values-> lexems nil)]
          (recur lexems (conj tree (conj propvalues propname))))))
    [lexemss tree]))


;; parse input file into tree structure
(defn process-tree-> [lexems tree]
  (let [[lexem & lexems] lexems]
    (condp = lexem
      :treestart (let [[lexems subtree] (process-tree-> lexems [])]
                   (recur lexems (conj tree subtree)))
      :nodestart (let [[lexems subtree] (process-node-> lexems [])]
                   (recur lexems (conj tree subtree)))
      :treeend [lexems tree]
      tree)))


;; process lexems seq into tree
(defnp parser-> [lexems]
  (process-tree-> lexems []))


(defnp parse [filename]
  (-> filename
      lazy-file-chars
      ->lexer
      parser->))
