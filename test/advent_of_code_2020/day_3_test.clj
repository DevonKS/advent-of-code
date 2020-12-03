(ns advent-of-code-2020.day-3-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-3 :refer :all]))

(deftest day-1-part-1-test
  (testing "Sample Data"
    (let [lines (string/split  "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#" #"\n")
          grid (parse-lines lines)]
      (is (= 7
             (challenge-1 grid)))))
  (testing "real-data"
    (is (= 162
           (challenge-1!)))))

(deftest day-1-part-2-test
  (testing "Sample Data"
    (let [lines (string/split  "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#" #"\n")
          grid (parse-lines lines)]
      (is (= 336
             (challenge-2 grid)))))
  (testing "real-data"
    (is (= 3064612320
           (challenge-2!)))))
