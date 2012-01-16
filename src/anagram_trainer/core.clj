(ns anagram-trainer.core
  (:require [clojure.string :as str])
  (:use [clojure.test]))

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

(def words-of-interest
  (set (flatten (map (comp str/split-lines slurp)
                     files-of-interest))))

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
  (is (= (sort-word "dcba") "abcd")))

(deftest
  test-anagrammap
  (let [m (anagram-map ["aa" "ab" "ba" "cab" "bac" "bca"])]
    (is (= (m "aa") ["aa"]))
    (is (= (set (m "ab")) #{"ab" "ba"}))))

(deftest
  test-anagrams
  (let [as (anagrams ["aa" "ab" "ba" "cab" "bac" "bca"])]
    (is (= (set (map set as))
           #{#{"bca" "bac" "cab"} #{"ba" "ab"}}))))

;; add words

(defn add-words [words]
  (let [freqs (apply merge-with max (map frequencies words))
        pretty (comp str/join sort flatten)]
    (pretty (for [[ch n] freqs]
              (repeat n ch)))))

(deftest
  test-add-words
  (is (= (add-words ["ab" "cd"]) "abcd"))
  (is (= (add-words ["ab" "ad"]) "abd"))
  (is (= (add-words ["ab" "aa"]) "aab"))
  (is (= (add-words ["aa" "ab"]) "aab"))
  (is (= (add-words ["aa" "aa"]) "aa"))
  (is (= (add-words ["hello" "world"]) "dehllorw"))
  (is (= (add-words ["six" "sick" "sheep"]) "ceehikpsx")))

;; main

(defn -main [& args]
  (println (anagrams words-of-interest)))
