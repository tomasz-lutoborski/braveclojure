(ns clojure-noob.sec4)

(def filename "/home/tomek/Development/clojure/clojure-noob/resources/suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "split a line into rows of columns"
  [string]
  (map #(clojure.string/split % #",") (clojure.string/split string #"\n")))

(defn mapify
  "return seq of maps"
  [rows]
  (map (fn [unmapped-rows]
         (reduce (fn [row-map [key value]]
                   (assoc row-map key (convert key value)))
                 {}
                 (map vector vamp-keys unmapped-rows)))
       rows))

(defn glitter-filter
  [minimum records]
  (filter #(>= (:glitter-index %) minimum) records))

(defn glitter-filter-names
  [minimum records]
  (map :name (glitter-filter minimum records)))

(defn append
  [suspects new-suspect]
  (conj suspects (:name new-suspect)))

(def validations {:name (fn [name] (not (empty? name)))
                  :glitter-index (fn [glitter-index] (>= glitter-index 0))})

(defn validate
  [suspect]
  (every? (fn [key] ((get validations key) (suspect key))) vamp-keys))

(defn suspects-map-to-csv
  [suspects]
  (clojure.string/join "\n"
                       (map (fn [suspect] (clojure.string/join ","
                                                               (map (fn [key] (str (suspect key))) vamp-keys))) suspects)))
