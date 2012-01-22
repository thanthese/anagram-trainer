(ns anagram-trainer.test.core
  (:use [anagram-trainer.core])
  (:use [clojure.test])
  (:require [clojure.java.io :as io]))

;; word sources

(deftest test-get-words-of-interest
  (is (> (count words-of-interest) 1500)))

;; anagram utils

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

(deftest
  test-add-words
  (is (= (add-words ["ab" "cd"]) "abcd"))
  (is (= (add-words ["ab" "ad"]) "abd"))
  (is (= (add-words ["ab" "aa"]) "aab"))
  (is (= (add-words ["aa" "ab"]) "aab"))
  (is (= (add-words ["aa" "aa"]) "aa"))
  (is (= (add-words ["hello" "world"]) "dehllorw"))
  (is (= (add-words ["six" "sick" "sheep"]) "ceehikpsx")))

(deftest
  test-subword?
  (is (subword? "ab" "bat"))
  (is (subword? "aba" "bata"))
  (is (subword? "a" "bata"))
  (is (subword? "hell" "hello world"))
  (is (subword? "lelh" "hello world"))
  (is (not (subword? "ab" "cat")))
  (is (not (subword? "aa" "cat"))))

(deftest
  test-find-sub-anagrams
  (let [pool words-of-interest]
    (is (= (set (sub-anagrams "cat" pool))
           #{"ta" "act" "at" "cat"}))
    (is (= (count (sub-anagrams "stephen" pool)) 31))
    (is (= (count (sub-anagrams "elizabeth" pool)) 72))))

;; related to state

(defn small-current-state []
  [{:word "tar" :score 0}
   {:word "end" :score 1}
   {:word "red" :score 2}
   {:word "big" :score 3}])

(defn medium-current-state []
  [{:word "tar" :score 0}
   {:word "end" :score 1}
   {:word "red" :score 2}
   {:word "bed" :score 3}
   {:word "ned" :score 4}
   {:word "say" :score -5}
   {:word "day" :score -6}
   {:word "ray" :score -7}
   {:word "may" :score -8}
   {:word "hex" :score -9}])

(deftest
  test-optimal-rack
  (is (= (optimal-rack (medium-current-state) 20 5) "amry")))

(deftest
  test-got
  (is (= (set (got ["tar" "big"] (small-current-state)))
         #{{:word "tar" :score 1}
           {:word "end" :score 1}
           {:word "red" :score 2}
           {:word "big" :score 4}})))

(deftest
  test-not-got
  (is (= (set (not-got ["tar" "big"] (small-current-state)))
         #{{:word "tar" :score -1}
           {:word "end" :score 1}
           {:word "red" :score 2}
           {:word "big" :score 2}})))

(deftest
  test-distance-from
  (is (= (distance-from 20 20) 0))
  (is (= (distance-from 20 10) 10))
  (is (= (distance-from 20 30) 10))
  (is (= (distance-from 20 -5) 25)))

;; persistence

(def test-store-path "/sandbox/test.clj")

(deftest
  test-initial-store
  (let [store (initial-state)]
    (is (> (count store) 1500))
    (is (= (first store)
           {:word "aa" :score 0}))
    (is (= 0 (apply + (map :score store))))))

(deftest
  test-read-store-returns-initial-state-when-no-file-is-found
  (do
    (io/delete-file test-store-path :silently)
    (is (= (read-store test-store-path)
         (initial-state)))))

(deftest
  test-write-store
  (let [state (small-current-state)]
    (do
      (write-store test-store-path state)
      (is (= state (read-store test-store-path))))))
