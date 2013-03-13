(ns org.thelastcitadel.bnfu.bootstrap
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]))

(defn characters []
  (with-open [r (io/input-stream (io/resource "example.bnf"))]
    (doall (map char (take-while (partial not= -1) (repeatedly #(.read r)))))))

(declare terminal
         literal
         rule-name
         opt-whitespace
         rule-symbol
         expression
         line-end
         rule
         bar
         elist
         syntax)

(defn literal [seq]
  (when (= \" (first seq))
    (let [x (take-while (partial not= \") (rest seq))
          y (drop (inc (inc (count x))) seq)]
      {:result [:literal (apply str x)]
       :rest y})))

(defn rule-name [seq]
  (when (= \< (first seq))
    (let [x (take-while (partial not= \>) (rest seq))
          y (drop (inc (inc (count x))) seq)]
      {:result [:rule-name (apply str x)]
       :rest y})))

(defn term [seq]
  (when (clojure.core/seq seq)
    (let [x (literal seq)]
      (if (:result x)
        {:result [:terminal (:result x)]
         :rest (:rest x)}
        (let [x (rule-name seq)]
          (if (:result x)
            {:result [:terminal (:result x)]
             :rest (:rest x)}
            nil))))))

(defn opt-whitespace [cs]
  (if (seq cs)
    (if (= \space (first cs))
      (recur (rest cs))
      {:rest cs
       :result [:opt-whitespace]})
    {:rest cs
     :result [:opt-whitespace]}))

(defn rule-symbol [cs]
  (when (= (seq "::=")
           (take 3 cs))
    {:result [:rule-symbol]
     :rest (drop 3 cs)}))

(defn bar [cs]
  (when (= (first cs) \|)
    {:result [:bar]
     :rest (rest cs)}))

(defn expression-left [cs]
  (let [x (elist cs)]
    (when (:result x)
      x)))

(defn concat-p [n parsers]
  (fn [cs]
    (loop [cs cs
           p parsers
           out []]
      (if (seq cs)
        (if (seq p)
          (let [x ((first p) cs)]
            (when (:result x)
              (recur (:rest x)
                     (rest p)
                     (conj out (:result x)))))
          {:result (vec (cons n out))
           :rest cs})
        (when-not (seq p)
          {:result (vec (cons n out))
           :rest cs})))))

(def expression-right
  (concat-p :expression [#'elist opt-whitespace bar opt-whitespace #'expression]))

(defn elist-left [cs]
  (let [x (term cs)]
    (when (:result x)
      x)))

(def elist-right
  (concat-p :elist [term opt-whitespace #'elist]))

(defn elist [cs]
  (let [x (elist-right cs)]
    (if (:result x)
      x
      (let [x (elist-left cs)]
        (when (:result x)
          {:result [:elist (:result x)]
           :rest (:rest x)})))))

(defn expression [cs]
  (let [x (expression-right cs)]
    (if (:result x)
      x
      (let [x (expression-left cs)]
        (when (:result x)
          {:result [:expression (:result x)]
           :rest (:rest x)})))))

(defn line-end [cs]
  (when (= (first cs) \newline)
    {:result [:line-end]
     :rest (rest cs)}))

(def rule
  (concat-p :rule [opt-whitespace rule-name opt-whitespace rule-symbol
                   opt-whitespace expression line-end]))

(defn syntax-left [cs]
  (let [x (rule cs)]
    (when (:result x)
      x)))

(def syntax-right
  (concat-p :syntax [rule #'syntax]))

(defn syntax [cs]
  (let [x (syntax-right cs)]
    (if (:result x)
      x
      (let [x (syntax-left cs)]
        (when (:result x)
          {:result [:syntax (:result x)]
           :rest (:rest x)})))))

(defn parse [rule cs]
  (:result (rule cs)))

;;(parse syntax (characters))

(defn character-match [string s]
  (= (apply str (take (count string) s))
     string))

(def expand* nil)
(defmulti expand* (fn [x]
                    (if (coll? x)
                      (first x)
                      :default)))

(defmethod expand* :default [x]
  x
  #_(if (vector? x)
      `(~(symbol (name (first x))) ~'s)
      x))

(defmethod expand* :syntax [[_ rule syntax]]
  (if (and syntax
           (= `do (first syntax)))
    `(do
       ~rule
       ~@(rest syntax))
    (if syntax
      `(do
         ~rule
         ~syntax)
      rule)))

(defmethod expand* :rule [[_ rule-name _ expression _]]
  `(defn ~rule-name [~'s]
     (when (seq ~'s)
       (when-let [x# ~expression]
         {:rest (:rest x#)
          :result [~(keyword (name rule-name)) (:result x#)]}))))

(defmethod expand* :rule-name [[_ n]]
  (symbol (.replaceAll n " " "-")))

(defmethod expand* :terminal [[_ x]]
  (if (vector? x)
    `(when (character-match ~(second x) ~'s)
       {:result [:terminal [:literal ~(second x)]]
        :rest (drop ~(count (second x)) ~'s)})
    `(~x ~'s)))

(defn and-up [expressions]
  (if (> (count expressions) 1)
    (with-meta
      (reduce
       (fn [e x]
         `(when-let [r# ~e]
            (let [~'s (:rest r#)
                  r2# ~x]
              (when r2#
                {:rest (:rest r2#)
                 :result [(:result r#) (:result r2#)]}))))
       (first expressions)
       (rest expressions))
      {::type ::and})
    (first expressions)))

(defmethod expand* :expression [[_ & anded]]
  (if (= 1 (count anded))
    (first anded)
    (let [[f s] (split-with (partial not= [:bar]) anded)]
      (if (seq s)
        `(or ~(and-up (rest s))
             ~(and-up f))
        (and-up (for [e f
                      x (if (and (seq? e)
                                 (= ::and (::type (meta e))))
                          (rest e)
                          [e])]
                  x))))))

(defmethod expand* :elist [[_ & anded]]
  (if (= 1 (count anded))
    (first anded)
    (and-up anded)))

(defn expand [s]
  (walk/postwalk
   (fn [x]
     (if (and (vector? x)
              (keyword? (first x)))
       (expand* (vec (for [i x
                           ii (if (= i [:opt-whitespace])
                                []
                                [i])]
                       ii)))
       x))
   s))
