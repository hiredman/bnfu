(ns org.thelastcitadel.bnfu
  (:require [clojure.java.io :as io]
            [org.thelastcitadel.bnfu.generate :refer :all]
            [org.thelastcitadel.bnfu.bootstrap2 :refer [rule-name
                                                        EOL
                                                        doublequote
                                                        opt-text]]))

(bnf "bnfu/bnf.bnf")

(defn parse-bnf [sequable]
  (parse-syntax sequable))
