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

(defn anagrams
  "Return lists of words that are anagrams of each other."
  [words]
  (filter (fn [ls] (> (count ls) 1))
          (vals (reduce (fn [acc w]
                          (let [k (str/join (sort w))]
                            (assoc acc k (cons w (acc k)))))
                        {}
                        words))))

(deftest test-anagrams
  (let [as (anagrams ["aa" "ab" "ba" "cab" "bac" "bca"])]
    (is (= (set (map set as))
           #{#{"bca" "bac" "cab"} #{"ba" "ab"}}))))


;; main

(defn -main [& args]
  (println (anagrams words-of-interest)))
