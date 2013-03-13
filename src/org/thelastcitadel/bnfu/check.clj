(ns check)

(defn rule-name [cs]
  (let [rn (take-while (partial not= \>) cs)]
    {:result [[:rule-name (apply str rn)]]
     :rest (drop (count rn) cs)}))

(defn doublequote [cs]
  (when cs
    (when (= (first cs)  \")
      {:result [[:doublequote]]
       :rest (rest cs)})))

(def charset
  #{\> \< \: \= \| \space \-})

(defn text [cs]
  (when (seq cs)
    (let [x (take-while #(or (Character/isJavaIdentifierPart %)
                             (charset %))
                        cs)]
      (if (seq x)
        {:result [[:text (apply str x)]]
         :rest (drop (count x) cs)}
        {:result [:opt-text]
         :rest cs}))))

(defn opt-text [cs]
  (when (seq cs)
    (let [x (take-while #(or (Character/isJavaIdentifierPart %)
                             (charset %))
                        cs)]
      (if (seq x)
        {:result [[:text (apply str x)]]
         :rest (drop (count x) cs)}
        {:result [:opt-text]
         :rest cs}))))

(defn EOL [s]
  (when (seq s)
    (when (= \newline (first s))
      {:result [[:EOL]]
       :rest (rest s)})))

(declare syntax rule opt-whitespace expression line-end expression-list term literal comment)


(do
 (clojure.core/defn
  syntax
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/or
      (clojure.core/when-let
       [r__27004__auto__ (rule s)]
       (clojure.core/let
        [s (:rest r__27004__auto__) r2__27005__auto__ (syntax s)]
        (clojure.core/when
         r2__27005__auto__
         {:rest (:rest r2__27005__auto__),
          :result
          (clojure.core/vec
           (clojure.core/concat
            (:result r__27004__auto__)
            (:result r2__27005__auto__)))})))
      (rule s))]
    {:rest (:rest x__26980__auto__),
     :result [[:syntax (:result x__26980__auto__)]]})))
 (clojure.core/defn
  rule
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/when-let
      [r__27004__auto__ (opt-whitespace s)]
      (clojure.core/let
       [s
        (:rest r__27004__auto__)
        r2__27005__auto__
        (clojure.core/when-let
         [r__27004__auto__

          (clojure.core/when
           (org.thelastcitadel.bnfu.bootstrap/character-match "<" s)
           {:rest (clojure.core/drop 1 s),
            :result [[:terminal [:literal "<"]]]})]
         (clojure.core/let
          [s
           (:rest r__27004__auto__)
           r2__27005__auto__
           (clojure.core/when-let
            [r__27004__auto__ (rule-name s)]
            (clojure.core/let
             [s
              (:rest r__27004__auto__)
              r2__27005__auto__
              (clojure.core/when-let
               [r__27004__auto__
                (clojure.core/when
                 (org.thelastcitadel.bnfu.bootstrap/character-match
                  ">"
                  s)
                 {:rest (clojure.core/drop 1 s),
                  :result [[:terminal [:literal ">"]]]})]
               (clojure.core/let
                [s
                 (:rest r__27004__auto__)
                 r2__27005__auto__
                 (clojure.core/when-let
                  [r__27004__auto__ (
opt-whitespace s)]
                  (clojure.core/let
                   [s
                    (:rest r__27004__auto__)
                    r2__27005__auto__
                    (clojure.core/when-let
                     [r__27004__auto__
                      (clojure.core/when
                       (org.thelastcitadel.bnfu.bootstrap/character-match
                        "::="
                        s)
                       {:rest (clojure.core/drop 3 s),
                        :result [[:terminal [:literal "::="]]]})]
                     (clojure.core/let
                      [s
                       (:rest r__27004__auto__)
                       r2__27005__auto__
                       (clojure.core/when-let
                        [r__27004__auto__ (opt-whitespace s)]
                        (clojure.core/let
                         [s
                          (:rest r__27004__auto__)
                          r2__27005__auto__
                          (clojure.core/when-let
                           
[r__27004__auto__ (expression s)]
                           (clojure.core/let
                            [s
                             (:rest r__27004__auto__)
                             r2__27005__auto__
                             (line-end s)]
                            (clojure.core/when
                             r2__27005__auto__
                             {:rest (:rest r2__27005__auto__),
                              :result
                              (clojure.core/vec
                               (clojure.core/concat
                                (:result r__27004__auto__)
                                (:result r2__27005__auto__)))})))]
                         (clojure.core/when
                          r2__27005__auto__
                          {:rest (:rest r2__27005__auto__),
                           :result
                           (clojure.core/vec
                            (clojure.core/concat
                             (:result r__27004__auto__)
                             
(:result r2__27005__auto__)))})))]
                      (clojure.core/when
                       r2__27005__auto__
                       {:rest (:rest r2__27005__auto__),
                        :result
                        (clojure.core/vec
                         (clojure.core/concat
                          (:result r__27004__auto__)
                          (:result r2__27005__auto__)))})))]
                   (clojure.core/when
                    r2__27005__auto__
                    {:rest (:rest r2__27005__auto__),
                     :result
                     (clojure.core/vec
                      (clojure.core/concat
                       (:result r__27004__auto__)
                       (:result r2__27005__auto__)))})))]
                (clojure.core/when
                 r2__27005__auto__
                 {:rest (:rest r2__27005__auto__),
                  :result
                  (clojure.core/vec
                   (clojure.core/concat
                    (:result r__27004__auto__
)
                    (:result r2__27005__auto__)))})))]
             (clojure.core/when
              r2__27005__auto__
              {:rest (:rest r2__27005__auto__),
               :result
               (clojure.core/vec
                (clojure.core/concat
                 (:result r__27004__auto__)
                 (:result r2__27005__auto__)))})))]
          (clojure.core/when
           r2__27005__auto__
           {:rest (:rest r2__27005__auto__),
            :result
            (clojure.core/vec
             (clojure.core/concat
              (:result r__27004__auto__)
              (:result r2__27005__auto__)))})))]
       (clojure.core/when
        r2__27005__auto__
        {:rest (:rest r2__27005__auto__),
         :result
         (clojure.core/vec
          (clojure.core/concat
           (:result r__27004__auto__)
           (:result r2__27005__auto__)))})))]
    {:rest (:rest x__26980__auto__),
     :result [[:rule (:result x__26980__auto__)]]})))
 (clojure.core/defn
  opt-whitespace
  [s]
  
(clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/or
      (clojure.core/when-let
       [r__27004__auto__
        (clojure.core/when
         (org.thelastcitadel.bnfu.bootstrap/character-match " " s)
         {:rest (clojure.core/drop 1 s),
          :result [[:terminal [:literal " "]]]})]
       (clojure.core/let
        [s
         (:rest r__27004__auto__)
         r2__27005__auto__
         (opt-whitespace s)]
        (clojure.core/when
         r2__27005__auto__
         {:rest (:rest r2__27005__auto__),
          :result
          (clojure.core/vec
           (clojure.core/concat
            (:result r__27004__auto__)
            (:result r2__27005__auto__)))})))
      (clojure.core/when
       (org.thelastcitadel.bnfu.bootstrap/character-match "" s)
       {:rest (clojure.core/drop 0 s),
        :result [[:terminal [:literal ""]]]}))]
    {:rest (:rest x__26980__auto__),
     :result [[:opt-whitespace (:result x__26980__auto__)]]})))
 (clojure.core/defn

  expression
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/or
      (clojure.core/when-let
       [r__27004__auto__ (expression-list s)]
       (clojure.core/let
        [s
         (:rest r__27004__auto__)
         r2__27005__auto__
         (clojure.core/when-let
          [r__27004__auto__ (opt-whitespace s)]
          (clojure.core/let
           [s
            (:rest r__27004__auto__)
            r2__27005__auto__
            (clojure.core/when-let
             [r__27004__auto__
              (clojure.core/when
               (org.thelastcitadel.bnfu.bootstrap/character-match
                "|"
                s)
               {:rest (clojure.core/drop 1 s),
                :result [[:terminal [:literal "|"]]]})]
             (clojure.core/let
              [s
               (:rest r__27004__auto__)
               r2__27005__auto__
               (clojure.core/when-let
                [r__27004__auto__ (opt-whitespace s)]
                
(clojure.core/let
                 [s
                  (:rest r__27004__auto__)
                  r2__27005__auto__
                  (expression s)]
                 (clojure.core/when
                  r2__27005__auto__
                  {:rest (:rest r2__27005__auto__),
                   :result
                   (clojure.core/vec
                    (clojure.core/concat
                     (:result r__27004__auto__)
                     (:result r2__27005__auto__)))})))]
              (clojure.core/when
               r2__27005__auto__
               {:rest (:rest r2__27005__auto__),
                :result
                (clojure.core/vec
                 (clojure.core/concat
                  (:result r__27004__auto__)
                  (:result r2__27005__auto__)))})))]
           (clojure.core/when
            r2__27005__auto__
            {:rest (:rest r2__27005__auto__),
             :result
             (clojure.core/vec
              (clojure.core/concat
               (:result r__27004__auto__
)
               (:result r2__27005__auto__)))})))]
        (clojure.core/when
         r2__27005__auto__
         {:rest (:rest r2__27005__auto__),
          :result
          (clojure.core/vec
           (clojure.core/concat
            (:result r__27004__auto__)
            (:result r2__27005__auto__)))})))
      (expression-list s))]
    {:rest (:rest x__26980__auto__),
     :result [[:expression (:result x__26980__auto__)]]})))
 (clojure.core/defn
  line-end
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__ (EOL s)]
    {:rest (:rest x__26980__auto__),
     :result [[:line-end (:result x__26980__auto__)]]})))
 (clojure.core/defn
  expression-list
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/or
      (clojure.core/when-let
       [r__27004__auto__ (term s)]
       (clojure.core/let
        [s
         (:rest r__27004__auto__)
         r2__27005__auto__
         (clojure.core/when-let
          
[r__27004__auto__ (opt-whitespace s)]
          (clojure.core/let
           [s
            (:rest r__27004__auto__)
            r2__27005__auto__
            (expression-list s)]
           (clojure.core/when
            r2__27005__auto__
            {:rest (:rest r2__27005__auto__),
             :result
             (clojure.core/vec
              (clojure.core/concat
               (:result r__27004__auto__)
               (:result r2__27005__auto__)))})))]
        (clojure.core/when
         r2__27005__auto__
         {:rest (:rest r2__27005__auto__),
          :result
          (clojure.core/vec
           (clojure.core/concat
            (:result r__27004__auto__)
            (:result r2__27005__auto__)))})))
      (term s))]
    {:rest (:rest x__26980__auto__),
     :result [[:expression-list (:result x__26980__auto__)]]})))
 (clojure.core/defn
  term
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/or
      (clojure.core/when-let
       
[r__27004__auto__
        (clojure.core/when
         (org.thelastcitadel.bnfu.bootstrap/character-match "<" s)
         {:rest (clojure.core/drop 1 s),
          :result [[:terminal [:literal "<"]]]})]
       (clojure.core/let
        [s
         (:rest r__27004__auto__)
         r2__27005__auto__
         (clojure.core/when-let
          [r__27004__auto__ (rule-name s)]
          (clojure.core/let
           [s
            (:rest r__27004__auto__)
            r2__27005__auto__
            (clojure.core/when
             (org.thelastcitadel.bnfu.bootstrap/character-match ">" s)
             {:rest (clojure.core/drop 1 s),
              :result [[:terminal [:literal ">"]]]})]
           (clojure.core/when
            r2__27005__auto__
            {:rest (:rest r2__27005__auto__),
             :result
             (clojure.core/vec
              (clojure.core/concat
               (:result r__27004__auto__)
               (:result r2__27005__auto__)))})))]
        (clojure.core/when
         r2__27005__auto__
         {:rest (:rest r2__27005__auto__),
          :result
          (clojure.core/vec
           (clojure.core/concat
            (:result r__27004__auto__)
            (:result r2__27005__auto__)))})))
      (literal s))]
    {:rest (:rest x__26980__auto__),
     :result [[:term (:result x__26980__auto__)]]})))
 (clojure.core/defn
  literal
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/when-let
      [r__27004__auto__ (doublequote s)]
      (clojure.core/let
       [s
        (:rest r__27004__auto__)
        r2__27005__auto__
        (clojure.core/when-let
         [r__27004__auto__ (text s)]
         (clojure.core/let
          [s
           (:rest r__27004__auto__)
           r2__27005__auto__
           (doublequote s)]
          (clojure.core/when
           r2__27005__auto__
           {:rest (:rest r2__27005__auto__),
            :result
            (clojure.core/vec
             (clojure.core/concat
              (:result r__27004__auto__
)
              (:result r2__27005__auto__)))})))]
       (clojure.core/when
        r2__27005__auto__
        {:rest (:rest r2__27005__auto__),
         :result
         (clojure.core/vec
          (clojure.core/concat
           (:result r__27004__auto__)
           (:result r2__27005__auto__)))})))]
    {:rest (:rest x__26980__auto__),
     :result [[:literal (:result x__26980__auto__)]]})))
 (clojure.core/defn
  comment
  [s]
  (clojure.core/when
   (clojure.core/seq s)
   (clojure.core/when-let
    [x__26980__auto__
     (clojure.core/when-let
      [r__27004__auto__
       (clojure.core/when
        (org.thelastcitadel.bnfu.bootstrap/character-match "--" s)
        {:rest (clojure.core/drop 2 s),
         :result [[:terminal [:literal "--"]]]})]
      (clojure.core/let
       [s
        (:rest r__27004__auto__)
        r2__27005__auto__
        (clojure.core/when-let
         [r__27004__auto__ (opt-text s)]
         (clojure.core/let
          [s (:rest r__27004__auto__) r2__27005__auto__ (line-end s)]
          (clojure.core/when
           r2__27005__auto__
           {:rest (:rest r2__27005__auto__),
            :result
            (clojure.core/vec
             (clojure.core/concat
              (:result r__27004__auto__)
              (:result r2__27005__auto__)))})))]
       (clojure.core/when
        r2__27005__auto__
        {:rest (:rest r2__27005__auto__),
         :result
         (clojure.core/vec
          (clojure.core/concat
           (:result r__27004__auto__)
           (:result r2__27005__auto__)))})))]
    {:rest (:rest x__26980__auto__),
     :result [[:comment (:result x__26980__auto__)]]}))))
