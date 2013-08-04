(ns org.thelastcitadel.bnfu.bootstrap2
  (:require [clojure.java.io :as io]))

(defn characters []
  (with-open [r (io/input-stream (io/resource "example.bnf"))]
    (doall (map char (take-while (partial not= -1) (repeatedly #(.read r)))))))

(defn parse [parse-stream rules outer-result]
  (if (seq parse-stream)
    (if (seq rules)
      (let [[rule & rules] rules
            result (rule parse-stream)]
        (for [result result
              item (parse (:rest result)
                          rules
                          (concat outer-result (:result result)))]
          item))
      [{:result outer-result
        :rest parse-stream}])
    (when-not (seq rules)
      [{:result outer-result
        :rest parse-stream}])))

(defn lit [value]
  (let [size (count value)
        vs (seq value)]
    (fn [parse-stream]
      (let [x (take size parse-stream)]
        (when (= x vs)
          [{:result [[value]]
            :rest (drop size parse-stream)}])))))

(defn opt-text [parse-stream]
  (let [cs (take-while #(or (Character/isJavaIdentifierPart %)
                            (#{\> \< \: \= \| \space \-} %)) parse-stream)]
    [{:result [[:opt-text (apply str cs)]]
      :rest (drop (count cs) parse-stream)}]))

(defn opt-whitespace [parse-stream]
  (let [cs (take-while #(Character/isWhitespace %) parse-stream)]
    [{:result [[:opt-whitespace]]
      :rest (drop (count cs) parse-stream)}]))

(defn EOL [parse-stream]
  (when (= \newline (first parse-stream))
    [{:result [[:EOL]]
      :rest (rest parse-stream)}]))

(defn wrap [name result-seq]
  (for [result result-seq]
    (update-in result [:result] (comp list (partial cons name) seq))))

(defn line-end [parse-stream]
  (wrap :line-end
        (concat (parse parse-stream
                       [opt-whitespace
                        EOL]
                       [])
                (parse parse-stream
                       [EOL]
                       []))))

(defn bnf-comment [parse-stream]
  (wrap :bnf-comment
        (parse parse-stream
               [(lit "--")
                opt-text
                line-end]
               [])))

(defn literal [parse-stream]
  (wrap :literal
        (parse parse-stream
               [(lit "\"")
                opt-text
                (lit "\"")]
               [])))

(defn rule-name [cs]
  (let [rn (take-while (partial not= \>) cs)]
    [{:result [[:rule-name (apply str rn)]]
      :rest (drop (count rn) cs)}]))

(defn term [parse-stream]
  (wrap :term
        (concat
         (parse parse-stream
                [(lit "<")
                 rule-name
                 (lit ">")]
                [])
         (parse parse-stream
                [literal]
                []))))

(defn expression-list [parse-stream]
  (wrap :expression-list
        (concat (parse parse-stream
                       [term
                        opt-whitespace
                        expression-list]
                       [])
                (parse parse-stream
                       [term]
                       []))))

(defn expression [parse-stream]
  (wrap :expression
        (concat (parse parse-stream
                       [expression-list
                        opt-whitespace
                        (lit "|")
                        opt-whitespace
                        expression]
                       [])
                (parse parse-stream
                       [expression-list]
                       []))))

(defn rule [parse-stream]
  (wrap :rule
        (parse parse-stream
               [opt-whitespace
                (lit "<")
                rule-name
                (lit ">")
                opt-whitespace
                (lit "::=")
                opt-whitespace
                expression
                line-end]
               [])))

(defn syntax [parse-stream]
  (wrap :syntax
        (concat (parse parse-stream
                       [rule
                        syntax]
                       [])
                (parse parse-stream
                       [rule]
                       []))))
