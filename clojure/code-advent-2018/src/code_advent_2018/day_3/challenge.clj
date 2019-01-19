(ns code-advent-2018.day-3.challenge)

(defn parse-claim
  [claim]
  (let [claim-parts (clojure.string/split claim #" ")
        id (clojure.string/replace (first claim-parts) #"#" "")
        start-location-parts (clojure.string/split (nth claim-parts 2) #",")
        start-x (Integer. (first start-location-parts))
        start-y (Integer. (clojure.string/replace (second start-location-parts) #":" ""))
        size-parts (clojure.string/split (nth claim-parts 3) #"x")
        width (Integer. (first size-parts))
        height (Integer. (second size-parts))]
    {:id id :start-x start-x :start-y start-y :width width :height height}))

(defn get-claim-points
  [claim]
  (let [start-x (:start-x claim)
        start-y (:start-y claim)
        x-points (range start-x (+ start-x (:width claim)))
        y-points (range start-y (+ start-y (:height claim)))]
    (reduce (fn
              [result x-point]
              (apply conj result
                     (map (fn
                            [y-point]
                            (vector x-point y-point))
                          y-points)))
            []
            x-points)))

(defn challenge1
  [filename]
  (let [file-string (slurp (str "src/code_advent_2018/day_3/" filename))
        claims (map parse-claim (clojure.string/split-lines file-string))
        claimed-points (reduce (fn [points claim]
                                 (apply conj points (get-claim-points claim)))
                               []
                               claims)
        frequencies-of-claimed-points (frequencies claimed-points)
        points-with->1-claims (count (filter #(>= (second %) 2) frequencies-of-claimed-points))]
    points-with->1-claims))

(defn challenge2
  [filename]
  (let [file-string (slurp (str "src/code_advent_2018/day_3/" filename))
        claims (map parse-claim (clojure.string/split-lines file-string))
        claimed-points (reduce (fn [points claim]
                                 (apply conj points (get-claim-points claim)))
                               []
                               claims)
        frequencies-of-claimed-points (frequencies claimed-points)]
    (reduce (fn
              [result claim]
              (if (every? #(= 1 (get frequencies-of-claimed-points %)) (get-claim-points claim))
                (reduced (:id claim))
                ""))
            ""
            claims)))