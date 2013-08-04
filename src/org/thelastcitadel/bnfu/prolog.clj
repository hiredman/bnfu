(ns org.thelastcitadel.bnfu.prolog
  (:require [clojure.java.io :as io]
            [org.thelastcitadel.bnfu.generate :refer :all]
            [org.thelastcitadel.bnfu.primitives :refer :all]
            [org.thelastcitadel.bnfu.bootstrap2 :refer [bnf]]))

(defn lowercase-letter [parse-stream]
  (when (seq parse-stream)
    (if (> 123 (int (first parse-stream)) 96)
      [{:result [[:lowercase-letter (first parse-stream)]]
        :rest (rest parse-stream)}])))

(defn uppercase-letter [parse-stream]
  (when (seq parse-stream)
    (if (> 91 (int (first parse-stream)) 64)
      [{:result [[:uppercase-letter (first parse-stream)]]
        :rest (rest parse-stream)}])))

(bnf "prolog.bnf")

(comment

  (parse-program
   "male(james1)."))
