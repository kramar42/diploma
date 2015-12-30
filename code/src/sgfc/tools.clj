(ns sgfc.tools)


(defn gen-letters [start end]
  ((comp set doall map) char (range (int start) (inc (int end)))))
