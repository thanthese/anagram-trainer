(ns anagram-trainer.core
  (:require [clojure.string :as str]))

;; get words

(def dict "resources/dict/OWL2.txt")

(def files-of-interest
  ["resources/lists/2-letter-words.txt"
   "resources/lists/3-letter-words.txt"
   "resources/lists/4-letters-3-vowels.txt"
   "resources/lists/5-letters-4-vowels.txt"
   "resources/lists/all-consts.txt"
   "resources/lists/q-words-no-qu.txt"
   "resources/lists/up-to-4-letter-j-words.txt"
   "resources/lists/up-to-4-letter-x-words.txt"
   "resources/lists/up-to-4-letter-z-words.txt"
   "resources/lists/up-to-5-letter-q-words.txt"])

(defn grab-words [path]
  (str/split-lines (slurp path)))

(def words-of-interest
  (set (flatten (map grab-words files-of-interest))))

;; find anagrams

(def sort-word (comp str/join sort))

(defn register-word [m word]
  (let [k (str/join (sort word))]
    (assoc m k (cons word (m k)))))

(defn anagram-map [words]
  (reduce register-word {} words))

(defn anagrams [words]
  (filter (fn [ls] (> (count ls) 1))
          (vals (anagram-map words))))

;; add words

(defn add-words [words]
  (let [freqs (apply merge-with max (map frequencies words))
        pretty (comp str/join sort flatten)]
    (pretty (for [[ch n] freqs]
              (repeat n ch)))))

;; word is in word

(defn subword? [sub sup]
  (let [freq-sub (frequencies sub)
        freq-sup (frequencies sup)]
    (every? (fn [[ch n]]
              (<= n (get freq-sup ch 0)))
            freq-sub)))

;; find all sub-anagrams in string

(defn sub-anagrams [super-word word-pool]
  (filter (fn [w] (subword? w super-word))
          word-pool))

;; main

(defn -main [& args]
  (println (anagrams words-of-interest)))

;; test states

(defn random-current-state []
  (for [w words-of-interest]
    {:word w :score (first (shuffle (range -100 101)))}))

(defn small-current-state []
  [{:word "tar" :score 0}
   {:word "end" :score 1}
   {:word "red" :score 2}
   {:word "big" :score 3}])

;; pick words

(defn next-letters [state]
  (add-words (map :word (take 2 (sort-by :score state)))))

;; adjust score

(defn match [op words state]
  (map
    (fn [entry]
      (if (some (partial = (:word entry)) words)
        (update-in entry [:score] op)
        entry))
    state))

(def got     (partial match inc))
(def not-got (partial match dec))
