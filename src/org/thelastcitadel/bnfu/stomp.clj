(ns org.thelastcitadel.bnfu.stomp
  (:require [clojure.java.io :as io]
            [org.thelastcitadel.bnfu.generate :refer [bnf]]
            [org.thelastcitadel.bnfu.bootstrap2]))

(defn EOL [parse-stream]
  (when (= \newline (first parse-stream))
    [{:result [[:EOL]]
      :rest (rest parse-stream)}]))

(defn CR [parse-stream]
  (when (and (seq parse-stream)
             (= \return (first parse-stream)))
    [{:result [[:CR]]
      :rest (rest parse-stream)}]))

(defn LF [parse-stream]
  (when (and (seq parse-stream)
             (= \newline (first parse-stream)))
    [{:result [[:LF]]
      :rest (rest parse-stream)}]))

(defn octets [parse-stream]
  (let [os (take-while #(not= 0 (int %)) parse-stream)]
    [{:result [[:octets (apply str os)]]
      :rest (drop (count os) parse-stream)}]))

(defn NULL [parse-stream]
  (when (and (seq parse-stream)
             (= 0 (int (first parse-stream))))
    [{:result [[:NULL]]
      :rest (rest parse-stream)}]))

(defn header-name [parse-stream]
  (let [os (take-while #(not= % \:) parse-stream)]
    [{:result [[:header-name(apply str os)]]
      :rest (drop (count os) parse-stream)}]))

(defn header-value [parse-stream]
  (let [os (take-while #(not= % \newline) parse-stream)]
    [{:result [[:header-value (apply str os)]]
      :rest (drop (count os) parse-stream)}]))

(def parse-frame)
(bnf "bnfu/stomp.bnf")

(comment

  (parse-frame
   "MESSAGE
subscription:0
message-id:007
destination:/queue/a
content-type:text/plain

hello queue a "))
