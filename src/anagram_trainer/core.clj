(ns anagram-trainer.core
  (:require [clojure.string :as str])
  (:import [java.io File]))

;; word sources

(def dict "Path to our official word list."
  "resources/dict/OWL2.txt")

(def word-files-of-interest
  "A list of word files of high-value scrabble words."
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

(defn grab-words
  "Get a vector of all words from a path.

  It is assumed that the file will contain one word per line."
  [path]
  (str/split-lines (slurp path)))

(def words-of-interest "A list of all words of interest."
  (set (flatten (map grab-words word-files-of-interest))))

;; anagram utils

(defn sort-word
  "Sort the letters in a word.  Ex: 'dcba' -> 'abcd'."
  [word]
  (str/join (sort word)))

(defn anagram-map
  "Take a list of words, and a return a map of sorted-words to anagrams.  Ex:

  [ab ba cd dc]  ->  { ab [ab ba]
                       cd [cd dc] }"
  [words]
  (let [register-word (fn [m w]
                        (let [s (sort-word w)]
                          (assoc m s (cons w (m s)))))]
    (reduce register-word {} words)))

(defn anagrams
  "Take a list of words, return a list of anagram lists.  Words with no
  anagrams are not included.  Ex:

  [a ab ba cd dc]  ->  [[ab ba] [cd dc]]"
  [words]
  (filter (fn [ls] (> (count ls) 1))
          (vals (anagram-map words))))

(defn add-words
  "Take the 'prime factorization' of a list of words.  Ex:

  'hello' + 'world' = 'dehllorw'"
  [words]
  (let [freqs (apply merge-with max (map frequencies words))
        pretty (comp sort-word flatten)]
    (pretty (for [[ch n] freqs]
              (repeat n ch)))))

(defn subword?
  "True if the superword contains the subword.  Ex: 'ab' is in 'bat'."
  [sub sup]
  (let [freq-sub (frequencies sub)
        freq-sup (frequencies sup)]
    (every? (fn [[ch n]]
              (<= n (get freq-sup ch 0)))
            freq-sub)))

(defn sub-anagrams
  "Return list of all words in word-pool that are contained within --
  sub-anagrams -- of super-word.  Ex: (assuming a pool of all scrabble words)

  'cat'  ->  ['ta' 'act' 'at' 'cat']."
  [super-word word-pool]
  (filter (fn [w] (subword? w super-word))
          word-pool))

;; main

(defn -main [& args]
  (println (anagrams words-of-interest)))

;; pick words

(defn distance-from [target attempt]
  (Math/abs (- target attempt)))

(def target-anagram-count 20)
(def bottom 5)
(defn optimal-rack
  "Return the optimal rack for the current state.  Ex: 'amnoy'.

  The ideal rack would ideally:
  1.  Guarantee having low-scoring-word solutions.
  2.  Have a reasonable number of solutions.

  Thus the process.  Take several (bottom) of the lowest scoring words.
  Cross-multiply them all against each other, and find out which resulting pair
  has the most favorable (closest to target-opts) number of sub-anagrams.
  Return those words, added together."
  [state target-opts bottom]
  (let [lowest-scoring-words (map :word (take bottom (sort-by :score state)))
        combos (for [a lowest-scoring-words
                     b lowest-scoring-words
                     :when (not= a b)]
                 (let [added (add-words [a b])
                       n (count (sub-anagrams added words-of-interest))]
                   {:added added
                    :dist (distance-from target-opts n)}))]
    (:added (first (sort-by :dist combos)))))

;; adjust score

(defn match [op words state]
  (map
    (fn [entry]
      (if (some (partial = (:word entry)) words)
        (update-in entry [:score] op)
        entry))
    state))

(defn got
  "Modify state to show that the user knew -- that is, 'got' -- the list of
  words."
  [words state]
  (match inc words state))

(defn not-got
  "Opposite of 'got'."
  [words state]
  (match dec words state))

;; persistence

(defn initial-state
  "What the state is initialized to if no store is found."
  []
  (for [w words-of-interest]
    {:word w :score 0}))

(defn read-store
  "Return state as saved in save-path.  If no such file exists, return an
  initial, default state."
  [save-path]
  (if (.exists (File. save-path))
    (read-string (slurp save-path))
    (initial-state)))

(defn write-store
  "Write state to store."
  [save-path state]
  (spit save-path state))
