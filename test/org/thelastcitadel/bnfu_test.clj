(ns org.thelastcitadel.bnfu-test
  (:require [clojure.test :refer :all]
            [org.thelastcitadel.bnfu.bnf :as bnf]
            [org.thelastcitadel.bnfu.stomp :as stomp]
            [org.thelastcitadel.bnfu.prolog :as prolog]
            [org.thelastcitadel.bnfu.primitives :as prim]
            [clojure.java.io :as io]))

(deftest t-bnf
  (let [[{:keys [rest result]}] (prim/parse (slurp (io/resource "bnfu/bnf.bnf"))
                                            [bnf/syntax]
                                            [])
        [r] result]
    (is (= () rest))
    (is (= 1 (count result)))
    (is (= :syntax (first r)))))

(def frame "MESSAGE
subscription:0
message-id:007
destination:/queue/a
content-type:text/plain

hello queue a ")

(deftest t-stomp
  (let [{:keys [rest result]} (stomp/parse-frame frame)
        [frame] result]
    (is (= () rest))
    (is (= :frame (first frame)))))

(def program
  "male(james1).
?- likes(mary,food).")

(deftest t-prolog
  (let [{:keys [rest result]} (prolog/parse-program program)
        [program] result]
    (is (= () rest))
    (is (= :program (first program)))))
