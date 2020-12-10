(ns advent-of-code-2020.day-10-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-10 :refer :all]))

(deftest day-10-part-1-test
  (testing "Sample Data 1"
    (let [input "16
10
15
5
1
11
7
19
6
12
4"
          lines (string/split input #"\n")
          nums (parse-lines lines)]
      (is (= 35
             (challenge-1 nums)))))
  (testing "Sample Data 2"
    (let [input "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"
          lines (string/split input #"\n")
          nums (parse-lines lines)]
      (is (= 220
             (challenge-1 nums)))))
  (testing "real-data"
    (is (= 2040
           (challenge-1!)))))

(deftest day-10-part-2-test
  (testing "Sample Data 1"
    (let [input "16
10
15
5
1
11
7
19
6
12
4"
          lines (string/split input #"\n")
          nums (parse-lines lines)]
      (is (= 8
             (challenge-2 nums)))))
  (testing "Sample Data 2"
    (let [input "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"
          lines (string/split input #"\n")
          nums (parse-lines lines)]
      (is (= 19208
             (challenge-2 nums)))))
  (testing "real-data"
    (is (= 28346956187648
           (challenge-2!)))))
