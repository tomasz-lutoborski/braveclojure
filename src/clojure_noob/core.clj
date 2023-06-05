(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn train
  []
  (println "Training..."))

(+ 1 1)

(if true
  (do (println "True")
      (println "True again"))
  (do (println "False")
      (println "False again")))

(defn communicate
  [target]
  (str "Hello, " target))

(defn communicate-with
  [& targets]
  (map communicate targets))

(defn favourite-things
  [name & things]
  (str name " likes " (clojure.string/join ", " things)))

(map (fn [name] (str "hello, " name)) ["bob" "jane" "joe"])

(defn inc-maker
  [inc-by]
  #(+ % inc-by))

(def inc-by-5 (inc-maker 5))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(def asym-alien-body-parts [{:name "head" :size 3}
                            {:name "eye-1" :size 1}
                            {:name "ear-1" :size 1}
                            {:name "mouth" :size 1}
                            {:name "nose" :size 1}
                            {:name "neck" :size 2}
                            {:name "shoulder-1" :size 3}
                            {:name "upper-arm-1" :size 3}
                            {:name "chest" :size 10}
                            {:name "back" :size 10}
                            {:name "forearm-1" :size 3}
                            {:name "abdomen" :size 6}
                            {:name "kidney-1" :size 1}
                            {:name "hand-1" :size 2}
                            {:name "knee-1" :size 2}
                            {:name "thigh-1" :size 4}
                            {:name "lower-leg-1" :size 3}
                            {:name "achilles-1" :size 1}
                            {:name "foot-1" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn matching-alien-part
  [part n]
  (if (re-matches #".*-1" (:name part))
    (let [body-part (first (clojure.string/split (:name part) #"-"))]
      (into '() (map (fn [n] {:name (str body-part "-" n) :size (:size part)}))
            (range 1 (inc n))))
    (list part)))

(defn symmetrize-alien-body-parts
  [asym-body-parts n]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set (matching-alien-part part n))))))))

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(def dalmatian-list
  ["Pongo" "Perdita" "Puppy 1" "Puppy 2"])

(let [dalmatians (take 2 dalmatian-list)]
  dalmatians)

(let [[pongo & dalmatians] dalmatian-list] [pongo dalmatians])

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(defn add-100
  [x]
  (+ x 100))

(defn dec-maker
  [dec-by]
  #(- % dec-by))

(def dec-by-5 (dec-maker 5))

(defn mapset
  [f  list]
  (set (map f list)))
