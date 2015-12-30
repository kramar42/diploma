(ns sgfc.core
  (:require [sgfc.sgf]))

;; MAIN
(defn -main [& args]
  (-> args
      first
      sgfc.sgf/parse))
