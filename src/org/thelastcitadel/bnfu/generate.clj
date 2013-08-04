(ns org.thelastcitadel.bnfu.generate
  (:require [clojure.java.io :as io]
            [org.thelastcitadel.bnfu.primitives :refer :all]))

(defmulti expression->clj first)

(defmethod expression->clj :expression [[_ & body]]
  {:post [%]}
  (case (count body)
    1 `(parse ~'parse-stream ~(vec (expression->clj (first body))) [])
    5 (let [[el _ _ _ ex] body
            a `~(vec (expression->clj el))
            b (expression->clj ex)]
        (if (or (and (seq? (first b))
                     (= (first (first b)) 'org.thelastcitadel.bnfu.primitives/parse-or))
                (= (first b) `for))
          (if (= (first b) `for)
            `(for [~'s [~@(second (second b))
                        (parse ~'parse-stream ~a [])]
                   ~'item ~'s]
               ~'item)
            `(for [~'s [(parse ~'parse-stream ~a [])
                        ~b]
                   ~'item ~'s]
               ~'item))
          (let [[_ _ b :as c] b
                _ (assert b c)
                c (sort-by (fn [rules] (- (count rules))) [a b])]
            `((parse-or ~@c) ~'parse-stream))))))

(defmethod expression->clj :expression-list [[_ & body]]
  {:post [%]}
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
  `(lit ~text))

(defn rule-exp->clj [[_ _ _ [_ rule-name] _ _ _ _ body _]]
  `(defrule ~(symbol (name rule-name)) [~'parse-stream]
     ~(expression->clj body)))

(defn syntax->rules [[tag & body]]
  {:pre [(= tag :syntax)
         (> 3 (count body) 0)]}
  (lazy-seq
   (case (count body)
     1 (seq body)
     2 (cons (first body)
             (syntax->rules (second body))))))

(intern (create-ns 'org.thelastcitadel.bnfu.bootstrap2) 'syntax)

(defn bnf->clojure [bnf]
  (let [rules (->> (parse (slurp (io/resource bnf)) [org.thelastcitadel.bnfu.bootstrap2/syntax] [])
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
