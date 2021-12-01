(ns advent-of-code-2020.day-7-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-7 :refer :all]))

(deftest day-7-part-1-test
  (testing "Sample Data"
    (let [graph (parse-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")]
      (is (= 4
             (challenge-1 graph)))))
  (testing "real-data"
    (is (= 164
           (challenge-1!)))))

(deftest day-7-part-2-test
  (testing "Sample Data"
    (let [graph (parse-input "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")]
      (is (= 126
             (challenge-2 graph)))))
  (testing "real-data"
    (is (= 7872
           (challenge-2!)))))
