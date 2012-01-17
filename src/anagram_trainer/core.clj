(ns anagram-trainer.core
  (:require [clojure.string :as str])
  (:use [clojure.test]))

;; "testing framework"

(defmacro fact [before _ after]
  `(is (= ~before ~after)))

(deftest
  who-tests-the-testers?
  (fact (+ 1 2) => 3))

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

(deftest test-get-words-of-interest
  (is (> (count words-of-interest) 1500)))

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

(deftest
  test-sort-word
  (fact (sort-word "dcba") => "abcd"))

(deftest
  test-anagrammap
  (let [m (anagram-map ["aa" "ab" "ba" "cab" "bac" "bca"])]
    (fact (m "aa") => ["aa"])
    (fact (set (m "ab")) => #{"ab" "ba"})))

(deftest
  test-anagrams
  (let [as (anagrams ["aa" "ab" "ba" "cab" "bac" "bca"])]
    (fact (set (map set as))
          =>
          #{#{"bca" "bac" "cab"} #{"ba" "ab"}})))

;; add words

(defn add-words [words]
  (let [freqs (apply merge-with max (map frequencies words))
        pretty (comp str/join sort flatten)]
    (pretty (for [[ch n] freqs]
              (repeat n ch)))))

(deftest
  test-add-words
  (fact (add-words ["ab" "cd"]) => "abcd")
  (fact (add-words ["ab" "ad"]) => "abd")
  (fact (add-words ["ab" "aa"]) => "aab")
  (fact (add-words ["aa" "ab"]) => "aab")
  (fact (add-words ["aa" "aa"]) => "aa")
  (fact (add-words ["hello" "world"]) => "dehllorw")
  (fact (add-words ["six" "sick" "sheep"]) => "ceehikpsx"))

;; word is in word

(defn subword? [sub sup]
  (let [freq-sub (frequencies sub)
        freq-sup (frequencies sup)]
    (every? (fn [[ch n]]
              (<= n (get freq-sup ch 0)))
            freq-sub)))

(deftest
  test-subword?
  (is (subword? "ab" "bat"))
  (is (subword? "aba" "bata"))
  (is (subword? "a" "bata"))
  (is (subword? "hell" "hello world"))
  (is (subword? "lelh" "hello world"))
  (is (not (subword? "ab" "cat")))
  (is (not (subword? "aa" "cat"))))

;; find all sub-anagrams in string

(defn sub-anagrams [super-word word-pool]
  (filter (fn [w] (subword? w super-word))
          word-pool))

(deftest
  test-find-sub-anagrams
  (let [pool words-of-interest]
    (fact (set (sub-anagrams "cat" pool))
          =>
           #{"ta" "act" "at" "cat"})
    (fact (count (sub-anagrams "stephen" pool)) => 31)
    (fact (count (sub-anagrams "elizabeth" pool)) => 72)))

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

(deftest
  test-next-letters
  (fact (next-letters (small-current-state)) => "adenrt"))
