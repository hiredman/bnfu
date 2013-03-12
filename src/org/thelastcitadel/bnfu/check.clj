(ns check)

(defn rule-name [cs]
  (let [rn (take-while (partial not= \>) cs)]
    {:result [:rule-name (apply str rn)]
     :rest (drop (count rn) cs)}))

(defn doublequote [cs]
  (when cs
    (when (= (first cs)  \")
      {:result [:doublequote]
       :rest (rest cs)})))

(def charset
  #{\> \< \: \=})

(defn text [cs]
  (when (seq cs)
    (let [x (take-while #(or (Character/isJavaIdentifierPart %)
                             (charset %))
                        cs)]
      (when (seq x)
        {:result [:text (apply str x)]
         :rest (drop (count x) cs)}))))

(defn opt-text [cs]
  (when (seq cs)
    (let [x (take-while #(Character/isJavaIdentifierPart %) cs)]
      (if (seq x)
        {:result [:text (apply str x)]
         :rest (drop (count x) cs)}
        {:result [:opt-text]
         :rest cs}))))

(defn EOL [s]
  (when (seq s)
    (when (= \newline (first s))
      {:result [:EOL]
       :rest (rest s)})))

(declare syntax rule opt-whitespace expression line-end expression-list term literal comment)



