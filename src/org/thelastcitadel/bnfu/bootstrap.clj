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
  (when (seq cs)
    (if (= \space (first cs))
      (recur (rest cs))
      {:rest cs
       :result [:opt-whitespace]})))

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
      (when (seq cs)
        (if (seq p)
          (let [x ((first p) cs)]
            (when (:result x)
              (recur (:rest x)
                     (rest p)
                     (conj out (:result x)))))
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
  (when (= (first cs)
           \newline)
    {:result [:line-ending]
     :rest (rest cs)}))

(def rule
  (concat-p :rule [opt-whitespace rule-name opt-whitespace rule-symbol
                   opt-whitespace expression line-end]))

(defn syntax-left [cs]
  (prn "syntax-left" (take 10 cs))
  (let [x (rule cs)]
    (when (:result x)
      x)))

(def syntax-right
  (concat-p :syntax [rule #'syntax]))

(defn syntax [cs]
  (prn "syntax" (take 10 cs))
  (let [x (syntax-right cs)]
    (if (:result x)
      x
      (let [x (syntax-left cs)]
        (when (:result x)
          {:result [:syntax (:result x)]
           :rest (:rest x)})))))

(defn parse [rule cs]
  (:result (rule cs)))

(defmulti rule-list first)

(defmethod rule-list :syntax [[_ b]]
  (if (vector? (first b))
    (mapcat rule-list b)
    (rule-list b)))

(defmethod rule-list :rule [a]
  [a])

(defn get-rule-name [[_ [_ e]]]
  (when (= (first e) :rule-name)
    (symbol (second e))))

(defmulti branch?* first)

(defn branch? [node]
  (and (vector? node)
       (keyword? (first node))
       (branch?* node)))

(defmethod branch?* :syntax [_] true)

(defmethod branch?* :rule [_] true)

(defmethod branch?* :opt-whitespace [_] false)

(defmethod branch?* :rule-name [_] false)

(defmethod branch?* :rule-symbol [_] false)

(defmethod branch?* :terminal [_] true)

(defmethod branch?* :literal [_] false)

(defmethod branch?* :bar [_] false)

(defmethod branch?* :line-ending [_] false)

(defmethod branch?* :expression [_] true)

(defmethod branch?* :elist [_] true)

(defmulti children first)

(defmethod children :syntax [[_ v]]
  (if (vector? (first v))
    v
    (list v)))

(defmethod children :rule [[_ v]] v)

(defmethod children :terminal [[_ v]] [v])

(defmethod children :expression [[_ v]] v)

(defmethod children :elist [[_ v]] v)

(defn make-node [& _]
  (throw (Exception.)))

(require '[clojure.zip :as z])

(def bnf-zip (partial z/zipper branch? children make-node))

(defn make-expression [s]
  (when (seq s)
    (let [[x & s] s]
      (case x
        :and (let [x1 (make-expression s)
                   x2 (make-expression (rest x1))
                   r (rest x2)
                   [x1] x1
                   [x2] x2]
               (cons `(let [_# '~'and
                            x1# ~x1
                            ~'cs (:rest x1#)]
                        (when (:result x1#)
                          (let [x2# ~x2]
                            (when (:result x2#)
                              {:result [(:result x1#)
                                        (:result x2#)]
                               :rest (:rest x2#)}))))
                     r))
        :or (let [x1 (make-expression s)
                  x2 (make-expression (rest x1))
                  r (rest x2)
                  [x1 x2] (sort-by count [(first x1)
                                          (first x2)])]
              (cons `(let [_# '~'or
                           x2# ~x2]
                       (if (:result x2#)
                         x2#
                         ~x1))
                    r))
        (cons x s)))))

(defn code-format [{:keys [rules code]}]
  `(do
     (declare ~@rules)
     ~@code))

;;(parse syntax (characters))
;; a weird little machine
(defn code-gen [ast]
  (loop [s ()
         e {}
         c (bnf-zip ast)
         d nil]
    (if (z/end? c)
      (if (seq d)
        (throw (Exception.))
        {:code (vals (apply merge s))
         :rules (keys e)})
      (let [n (z/node c)
            op (first n)]
        (case op
          :syntax (recur s e (z/next c) d)
          :opt-whitespace (recur s e (z/next c) d)
          :rule-symbol (recur s e (z/next c) d)
          :expression (recur s e (z/next c) d)
          :terminal (recur s e (z/next c) d)
          :elist (if (> (count (children n)) 1)
                   (recur (conj s :and) e (z/next c) d)
                   (recur s e (z/next c) d))
          :rule (do
                  (prn n)
                  (prn)
                  (recur () e (z/next c) s))
          :rule-name (let [[_ rule-name] n
                           rule-name (symbol (.replaceAll rule-name " " "-"))
                           e (assoc e rule-name nil)]
                       (if (contains? e :rule-name)
                         (recur (conj s (list rule-name 'cs))
                                e
                                (z/next c)
                                d)
                         (recur s
                                (assoc e :rule-name rule-name)
                                (z/next c)
                                d)))
          :bar (recur (list* (first s) :or (rest s))
                      e
                      (z/next c)
                      d)
          :line-ending (recur (conj d {(get e :rule-name)
                                       `(defn ~(get e :rule-name) [~'cs]
                                          (when (seq ~'cs)
                                            (let [x# ~(first (make-expression
                                                              (reverse s)))]
                                              (when x#
                                                {:result [~(keyword (name (get e :rule-name))) (:result x#)]
                                                 :rest (:rest x#)}))))})
                              (assoc (dissoc e :rule-name)
                                (get e :rule-name) nil)
                              (z/next c)
                              nil)
          :literal (recur (let [[_ x] n]
                            (if (empty? x)
                              (conj s
                                    `{:result [:literal ""]
                                      :rest ~'cs})
                              (conj s
                                    `(let [s# (seq ~x)
                                           x# (take ~(count x) ~'cs)]
                                       (when (= x# s#)
                                         {:result [:literal ~x]
                                          :rest (drop ~(count x) ~'cs)})))))
                          e
                          (z/next c)
                          d)
          (throw (Exception.)))))))

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

(defmethod expand* :rule [[_ _ rule-name _ _ _ expression]]
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
       {:result [:terminal ~(second x)]
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
        `(or ~(and-up f)
             ~(and-up (rest s)))
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

(defmethod expand* :opt-whitespace [_]
  `(~'opt-whitespace ~'s))

(defn expand [s]
  (walk/postwalk
   (fn [x]
     (if (and (vector? x)
              (keyword? (first x)))
       (expand* x)
       x))
   s))
