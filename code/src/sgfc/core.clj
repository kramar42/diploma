(ns sgfc.core
  (:gen-class))

(def UcLetters (doall (map char (range (int \A) (+ (int \Z) 1)))))

(defn lazy-read [file]
  (let [rdr (clojure.java.io/reader file)
        is-closed? (atom false)]
    (fn next-char []
      (if @is-closed?
        nil
        (let [chr (.read rdr)]
          (if (= -1 chr)
            (do (.close rdr)
                (swap! is-closed? complement)
                nil)
            (char chr)))))))

(defn read-char [lazy-read-fn stack]
  (if-let [result (first @stack)]
    (do
      (swap! stack rest)
      result)
    (lazy-read-fn)))

(defn unread-char [chr stack]
  (swap! stack (partial cons chr))
  chr)

(defn lexer [input-chars stack result]
  (letfn [(process-chars [input-chars stack result]
            (if-let [next-char (read-char input-chars stack)]
              (condp #(some #{%2} %1) next-char
                [\(] :treestart
                [\)] :treeend
                [\;] :nodestart
                UcLetters (list :propident
                                (apply str (cons next-char
                                                 (process-prop-ident input-chars stack))))
                [\[] (list :propvalue
                           (apply str (process-prop-value input-chars stack)))
                [\]] :propvalueend
                (recur input-chars stack result))
              nil))

          (process-prop-ident [input-chars stack]
            (if-let [next-char (read-char input-chars stack)]
              (if (some #{next-char} UcLetters)
                (cons next-char (process-prop-ident input-chars stack))
                (do (unread-char next-char stack) nil))))

          (process-prop-value [input-chars stack]
            ; teach to skip \] in values
            (if-let [next-char (read-char input-chars stack)]
              (if (= next-char \])
                (do (unread-char next-char stack) nil)
                (cons next-char (process-prop-value input-chars stack)))))]

    (if-let [next-token (process-chars input-chars stack result)]
      (recur input-chars stack (cons next-token result))
      result)))


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

(defn process-file [file-name]
  (let [lazy-read-fn (lazy-read file-name)
        stack (atom nil)
        lexems (reverse (lexer lazy-read-fn stack nil))
        tree   (parser lexems)]
    tree))

(defn simplify [tree]
  (if (instance? java.lang.String (first tree))
    ;; process property ident/value pair
    (let [color (first tree)
          move (second tree)]
      (if (or
            (= color "W")
            (= color "B"))
        move))
    ;; process list
    (filter (complement nil?) (apply list (map simplify tree)))))

(defn -main [& args]
  (process-file (first args)))