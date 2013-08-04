(ns org.thelastcitadel.bnfu.bootstrap2
  (:require [clojure.java.io :as io]
            [org.thelastcitadel.bnfu.generate :refer :all]
            [org.thelastcitadel.bnfu.primitives :refer :all]))

(defn characters []
  (with-open [r (io/input-stream (io/resource "example.bnf"))]
    (doall (map char (take-while (partial not= -1) (repeatedly #(.read r)))))))

(def doublequote (lit "\""))

(defn opt-text [parse-stream]
  (let [cs (take-while #(or (Character/isJavaIdentifierPart %)
                            (#{\> \< \: \= \| \space \-} %)) parse-stream)]
    [{:result [[:opt-text (apply str cs)]]
      :rest (drop (count cs) parse-stream)}]))

(defn EOL [parse-stream]
  (when (= \newline (first parse-stream))
    [{:result [[:EOL]]
      :rest (rest parse-stream)}]))

(defn rule-name [cs]
  (let [rn (take-while (partial not= \>) cs)]
    [{:result [[:rule-name (apply str rn)]]
      :rest (drop (count rn) cs)}]))

(declare opt-whitespace
         line-end
         bnf-comment
         literal
         term
         expression-list
         expression
         rule
         syntax)

(defrule
 syntax
 [parse-stream]
 ((parse-or [rule syntax] [rule]) parse-stream))

(defrule
 rule
 [parse-stream]
 (parse
  parse-stream
  [opt-whitespace
   (lit "<")
   rule-name
   (lit ">")
   opt-whitespace
   (lit "::=")
   opt-whitespace
   expression
   line-end]
  []))

(defrule whitespace [parse-stream] (parse parse-stream [(lit " ")] []))

(defrule
 opt-whitespace
 [parse-stream]
 ((parse-or [whitespace opt-whitespace] [(lit "")]) parse-stream))

(defrule
 expression
 [parse-stream]
 ((parse-or
   [expression-list opt-whitespace (lit "|") opt-whitespace expression]
   [expression-list])
  parse-stream))

(defrule
 line-end
 [parse-stream]
 ((parse-or [whitespace line-end] [EOL]) parse-stream))

(defrule
 expression-list
 [parse-stream]
 ((parse-or [term opt-whitespace expression-list] [term]) parse-stream))

(defrule
 term
 [parse-stream]
 ((parse-or [(lit "<") rule-name (lit ">")] [literal]) parse-stream))

(defrule
 literal
 [parse-stream]
 (parse parse-stream [doublequote opt-text doublequote] []))

(defrule
 bnf-comment
 [parse-stream]
 (parse parse-stream [(lit "--") opt-text line-end] []))

(defn bnf->clojure [bnf]
  (let [rules (->> (parse (slurp (io/resource bnf)) [syntax] [])
                   (first)
                   (:result)
                   (first)
                   (syntax->rules)
                   (map rule-exp->clj))]
    `(do
       (declare ~@(map second rules))
       ~@rules
       (defn ~(symbol (str "parse-" (name (second (first rules))))) [~'parse-stream]
         (first (parse ~'parse-stream [~(second (first rules))] []))))))

(defmacro bnf [i]
  (bnf->clojure i))

(bnf "example.bnf")
