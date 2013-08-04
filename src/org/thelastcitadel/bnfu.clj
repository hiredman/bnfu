(ns org.thelastcitadel.bnfu
  (:require [org.thelastcitadel.bnfu.generate :refer [bnf->clojure]]))

(defmacro bnf [i]
  (bnf->clojure i))
