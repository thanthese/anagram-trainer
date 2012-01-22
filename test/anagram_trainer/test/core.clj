(ns anagram-trainer.test.core
  (:use [anagram-trainer.core])
  (:use [clojure.test]))

(defn small-current-state []
  [{:word "tar" :score 0}
   {:word "end" :score 1}
   {:word "red" :score 2}
   {:word "big" :score 3}])

(deftest test-get-words-of-interest
  (is (> (count words-of-interest) 1500)))

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

(deftest
  test-next-letters
  (is (= (next-letters (small-current-state)) "adenrt")))

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
