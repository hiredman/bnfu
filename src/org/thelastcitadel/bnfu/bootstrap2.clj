(ns org.thelastcitadel.bnfu.bootstrap2
  (:require [clojure.java.io :as io]))

(defn characters []
  (with-open [r (io/input-stream (io/resource "example.bnf"))]
    (doall (map char (take-while (partial not= -1) (repeatedly #(.read r)))))))

(defn parse [parse-stream rules outer-result]
  (if (seq rules)
    (let [[rule & rules] rules
          result (rule parse-stream)]
      (for [result result
            item (parse (:rest result)
                        rules
                        (concat outer-result (:result result)))]
        item))
    [{:result outer-result
      :rest parse-stream}]))

(defn lit [value]
  (let [size (count value)
        vs (seq value)]
    (fn [parse-stream]
      (let [x (seq (take size parse-stream))]
        (when (= x vs)
          [{:result [[value]]
            :rest (drop size parse-stream)}])))))

(def doublequote (lit "\""))

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

(defn parse-or [a b]
  (fn [parse-stream]
    (concat (parse parse-stream a [])
            (parse parse-stream b []))))

(defmacro defrule [rule-name args & body]
  `(defn ~(symbol (name rule-name)) ~args
     (wrap ~(keyword (name rule-name))
           ~@body)))

(defrule line-end [parse-stream]
  ((parse-or [opt-whitespace
              EOL]
             [EOL])
   parse-stream))

(defrule bnf-comment [parse-stream]
  (parse parse-stream
         [(lit "--")
          opt-text
          line-end]
         []))

(defrule literal [parse-stream]
  (parse parse-stream
         [(lit "\"")
          opt-text
          (lit "\"")]
         []))

(defn rule-name [cs]
  (let [rn (take-while (partial not= \>) cs)]
    [{:result [[:rule-name (apply str rn)]]
      :rest (drop (count rn) cs)}]))

(defrule term [parse-stream]
  ((parse-or [(lit "<")
              rule-name
              (lit ">")]
             [literal])
   parse-stream))

(defrule expression-list [parse-stream]
  ((parse-or [term
              opt-whitespace
              expression-list]
             [term])
   parse-stream))

(defrule expression [parse-stream]
  ((parse-or [expression-list
              opt-whitespace
              (lit "|")
              opt-whitespace
              expression]
             [expression-list])
   parse-stream))

(defrule rule [parse-stream]
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
         []))

(defrule syntax [parse-stream]
  ((parse-or [rule
              syntax]
             [rule])
   parse-stream))

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


(defmulti expression->clj first)

(defmethod expression->clj :expression [[_ & body]]
  (case (count body)
    1 `(~'parse ~'parse-stream ~(vec (expression->clj (first body))) [])
    5 (let [[el _ _ _ ex] body
            a `~(vec (expression->clj el))
            [_ _ b] (expression->clj ex)
            c (sort-by (fn [rules] (- (count rules))) [a b])]
        `((~'parse-or ~@c) ~'parse-stream))))

(defmethod expression->clj :expression-list [[_ & body]]
  (case (count body)
    1 [(expression->clj (first body))]
    3 (let [[t _ el] body]
        (cons (expression->clj t)
              (expression->clj el)))))

(defmethod expression->clj :term [[_ & body]]
  (case (count body)
    1 (expression->clj (first body))
    3 (let [[_ [_ rule-name] _] body]
        (symbol rule-name))))

(defmethod expression->clj :literal [[_ _ [_ text] _]]
  `(~'lit ~text))

(defn rule-exp->clj [[_ _ _ [_ rule-name] _ _ _ _ body _]]
  `(~'defrule ~(symbol (name rule-name)) [~'parse-stream]
     ~(expression->clj body)))

(defn syntax->rules [[tag & body]]
  {:pre [(= tag :syntax)
         (> 3 (count body) 0)]}
  (lazy-seq
   (case (count body)
     1 (seq body)
     2 (cons (first body)
             (syntax->rules (second body))))))

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
         (:result (first (parse ~'parse-stream [~(second (first rules))] [])))))))

(defmacro bnf [i]
  (bnf->clojure i))

(bnf "example.bnf")

