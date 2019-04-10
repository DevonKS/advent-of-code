(ns code-advent-2018.day-13.challenge
  (:require [clojure.string :as s]))

(defn get-valid-directions
  [[x y] board]
  (let [current-char (get-in board [y x])]
    (cond
      (contains? #{\- \< \>} current-char) #{:left :right} ;I am assuming here that the < and > character will never start on an intersection
      (contains? #{\| \^ \v} current-char) #{:up :down} ;I am assuming here that the ^ and v character will never start on an intersection
      (= current-char \+) #{:left :up :right :down}
      (= current-char \\) (if (contains? #{\| \^ \v \+} (get-in board [(dec y) x])) #{:right :up} #{:left :down})
      (= current-char \/) (if (contains? #{\| \^ \v \+} (get-in board [(dec y) x])) #{:left :up} #{:right :down})
      :else #{})))

(defn get-rails
  [lines]
  (into []
        (map-indexed (fn [y line]
                       (into []
                             (map-indexed (fn [x _]
                                            (get-valid-directions [x y] lines))
                                          line)))
                     lines)))

(defn create-cart
  [position direction]
  {:start-position position
   :position position
   :direction direction
   :next-intersection-direction :left})

(defn get-cart
  [position char]
  (cond
    (= char \v) (create-cart position :down)
    (= char \^) (create-cart position :up)
    (= char \<) (create-cart position :left)
    (= char \>) (create-cart position :right)))

(defn get-carts
  [lines]
  (sort-by #(into [] (reverse (:position %)))
           (mapcat identity
                   (keep-indexed (fn [y line]
                                   (keep-indexed (fn [x char]
                                                   (get-cart [x y] char))
                                                 line))
                                 lines))))

(defn remove-opposite-direction
  [current-valid-directions direction]
  (cond
    (= direction :left) (disj current-valid-directions :right)
    (= direction :up) (disj current-valid-directions :down)
    (= direction :right) (disj current-valid-directions :left)
    (= direction :down) (disj current-valid-directions :up)))

(defn update-cart-position
  [cart direction]
  (let [cart (assoc cart :direction direction)]
    (cond
      (= direction :left) (update cart :position #(vector (dec (first %)) (second %)))
      (= direction :up) (update cart :position #(vector (first %) (dec (second %))))
      (= direction :right) (update cart :position #(vector (inc (first %)) (second %)))
      (= direction :down) (update cart :position #(vector (first %) (inc (second %)))))))

(defn update-next-intersection-direction
  [direction]
  (cond
    (= direction :left) :straight
    (= direction :straight) :right
    (= direction :right) :left))

(defn adjust-direction
  [direction adjust-clockwise?]
  (cond
    (= direction :left) (if adjust-clockwise? :up :down)
    (= direction :up) (if adjust-clockwise? :right :left)
    (= direction :right) (if adjust-clockwise? :down :up)
    (= direction :down) (if adjust-clockwise? :left :right)))

(defn get-next-intersection-direction
  [current-direction next-direction]
  (cond
    (= next-direction :straight) current-direction
    (= next-direction :left) (adjust-direction current-direction false)
    (= next-direction :right) (adjust-direction current-direction true)))

(defn move-cart
  [rails cart]
  (let [[cart-x cart-y] (:position cart)
        rails-index [cart-y cart-x]
        current-valid-directions (get-in rails rails-index)
        directions-cart-can-go (remove-opposite-direction current-valid-directions (:direction cart))]
    (if (= 1 (count directions-cart-can-go))
      (update-cart-position cart (first directions-cart-can-go))
      (-> cart
          (update-cart-position (get-next-intersection-direction (:direction cart) (:next-intersection-direction cart)))
          (update :next-intersection-direction update-next-intersection-direction)))))

(defn move-carts
  [rails carts]
  (mapv #(move-cart rails %) carts))

(defn get-first-colision
  [carts]
  (->> (map :position carts)
       (frequencies)
       (filter #(> (second %) 1))
       (sort-by #(into [] (reverse (first %))))
       (first)
       (first)))

(defn map-valid-direction-to-string
  [directions]
  (mapv #(cond
           (= % #{}) " "
           (= % #{:left :right}) "-"
           (= % #{:up :down}) "|"
           (or (= % #{:down :right}) (= % #{:left :up})) "/"
           (or (= % #{:down :left}) (= % #{:right :up})) "\\"
           (= % #{:up :right :down :left}) "+"
           :else " ")
        directions))

(defn print-rails
  [rails carts]
  (let [rails-characters (mapv map-valid-direction-to-string rails)
        rails-carts-character (reduce (fn [result cart]
                                        (let [cart-direction (:direction cart)
                                              cart-string (cond
                                                            (= cart-direction :up) "^"
                                                            (= cart-direction :right) ">"
                                                            (= cart-direction :down) "v"
                                                            (= cart-direction :left) "<")
                                              [cart-x cart-y] (:position cart)]
                                          (assoc-in result [cart-y cart-x] cart-string)))
                                      rails-characters
                                      carts)]
    (println (s/join "\n" (map #(s/join "" %) rails-carts-character)))))

(defn parse-file
  [filename]
  (let [lines (s/split-lines (slurp (str "src/code_advent_2018/day_13/" filename)))
        rails (get-rails lines)
        carts (get-carts lines)]
    [rails carts]))

(defn challenge1
  [filename]
  (let [[rails carts] (parse-file filename)]
    (loop [carts carts]
      (let [[carts collision] (reduce (fn [[carts collision] cart]
                                        (let [new-cart (move-cart rails cart)
                                              new-carts (conj (remove #(= % cart) carts) new-cart)
                                              collision (get-first-colision new-carts)]
                                          (if (nil? collision)
                                            [new-carts nil]
                                            (reduced [new-carts collision]))))
                                      [carts nil]
                                      carts)]
        (if (nil? collision)
          (recur (sort-by #(into [] (reverse (:position %))) carts))
          collision)))))

(defn challenge2
  [filename]
  (let [[rails carts] (parse-file filename)]
    (loop [carts carts]
      (let [[carts _] (reduce (fn [[carts collisions] cart]
                                (if (contains? collisions (:position cart))
                                  [carts collisions]
                                  (let [new-cart (move-cart rails cart)
                                        new-carts (conj (remove #(= % cart) carts) new-cart)
                                        collision (get-first-colision new-carts)
                                        new-carts-collisions-removed (remove #(= collision (:position %)) new-carts)]
                                    (if (>= 1 (count new-carts-collisions-removed))
                                      (reduced [new-carts-collisions-removed (if (nil? collision) collisions (conj collisions collision))])
                                      [new-carts-collisions-removed (if (nil? collision) collisions (conj collisions collision))]))))
                              [carts #{}]
                              carts)]
        (if (>= 1 (count carts))
          (:position (first carts))
          (recur (sort-by #(into [] (reverse (:position %))) carts)))))))