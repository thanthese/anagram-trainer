(ns anagram-trainer.core
  (:require [clojure.string :as str]))

(def dict "/Users/thanthese/scrabble-lists/OWL2.txt")



(def j "/Users/thanthese/scrabble-lists/up-to-4-letter-j-words.txt")
(def x "/Users/thanthese/scrabble-lists/up-to-4-letter-x-words.txt")
(def z "/Users/thanthese/scrabble-lists/up-to-4-letter-z-words.txt")
(def q "/Users/thanthese/scrabble-lists/up-to-5-letter-q-words.txt")
(def u "/Users/thanthese/scrabble-lists/q-words-no-qu.txt")
(def t "/Users/thanthese/scrabble-lists/2-letter-words.txt")
(def r "/Users/thanthese/scrabble-lists/3-letter-words.txt")

(defn grab-words [path] (str/split-lines (slurp path)))

(def words-of-interest (set (flatten (map grab-words [j x z q u t r]))))

(defn anagrams
  "Return lists of words that are anagrams of each other."
  [words]
  (filter (fn [ls] (> (count ls) 1))
          (vals (reduce (fn [acc w]
                          (let [k (str/join (sort w))]
                            (assoc acc k (cons w (acc k)))))
                        {}
                        words))))

(defn -main [& args]
  (println (anagrams words-of-interest))
  (println (grab-words "resources/lists/2-letter-words.txt")))
