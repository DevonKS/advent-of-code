(ns advent-of-code-2020.day-20
  (:require [advent-of-code-2020.util :as util]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as cset]
            [clojure.string :as string]))

(defn parse-tile
  [raw-tile]
  (let [lines (string/split-lines raw-tile)
        tile-id-string (first lines)
        [_ tile-id] (re-matches #"Tile (\d+):" tile-id-string)
        tile-id (util/parse-int tile-id)]
    {:id tile-id
     :image (mapv vec (rest lines))}))

(defn parse-tiles
  [raw-tiles]
  (mapv parse-tile raw-tiles))

(defn parse-input
  [input]
  (parse-tiles (string/split input #"\n\n")))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 20)))

(defn get-borders
  [tile]
  (let [image (:image tile)
        top (first image)
        bottom (peek image)
        left (mapv first image)
        right (mapv peek image)]
    {:top top
     :bottom bottom
     :left left
     :right right}))

(defn matching-border
  [tile-1 tile-2]
  (let [tile-1-borders (get-borders tile-1)
        tile-2-borders (get-borders tile-2)]
    (util/first-match
     #(= (get tile-1-borders (first %))
         (get tile-2-borders (second %)))
     [[:right :left] [:bottom :top] [:left :right] [:top :bottom]])))

(defn flip-rows
  [tile]
  (update tile :image (comp vec reverse)))

(defn flip-columns
  [tile]
  (update tile :image (fn [img] (mapv (comp vec reverse) img))))

(defn rotate-90-cw
  [tile]
  (let [img (:image tile)
        num-rows (count img)
        num-cols (count (first img))]
    (update tile :image
            (fn [img] (mapv (fn [row-idx]
                              (mapv
                               (fn [col-idx]
                                 (get-in img [(- (dec num-rows) col-idx) (- (dec num-cols) row-idx)]))
                               (range num-cols)))
                            (range num-rows))))))

(defn rotate-90-ccw
  [tile]
  (let [img (:image tile)
        num-rows (count img)
        num-cols (count (first img))]
    (update tile :image
            (fn [img] (mapv (fn [row-idx]
                              (mapv
                               (fn [col-idx]
                                 (get-in img [col-idx (- (dec num-cols) row-idx)]))
                               (range num-cols)))
                            (range num-rows))))))

(defn rotate-180
  [tile]
  ((comp flip-rows flip-columns) tile))

(defn orientation-matching-borders
  [orientation-fn tiles]
  (let [oriented-tiles (map orientation-fn tiles)
        tile-combs (combo/combinations oriented-tiles 2)]
    (reduce
     (fn [res [tile-1 tile-2]]
       (let [matching-border (matching-border tile-1 tile-2)]
         (if matching-border
           (assoc res [(:id tile-1) (:id tile-2)] matching-border)
           res)))
     {}
     tile-combs)))

(defn get-matching-tiles
  [tiles]
  (let [tile-combs (combo/combinations tiles 2)
        possible-orientations [[:id identity]
                               [:r90c rotate-90-cw]
                               [:r90cc rotate-90-ccw]
                               [:r180 rotate-180]
                               [:fr flip-rows]
                               [:fc flip-columns]
                               [:fr-r90c (comp flip-rows rotate-90-cw)]
                               [:fr-r90cc (comp flip-rows rotate-90-ccw)]]]
    (reduce
     (fn [res [tile-1 tile-2]]
       (let [matching-borders (remove nil?
                                      (for [orientation-1 possible-orientations ;[["id" identity]]
                                            orientation-2 possible-orientations]
                                        (let [[orientation-1-name orientation-1-fn] orientation-1
                                              [orientation-2-name orientation-2-fn] orientation-2
                                              matching-border (matching-border (orientation-1-fn tile-1)
                                                                               (orientation-2-fn tile-2))]
                                          (when matching-border
                                            [orientation-1-name orientation-2-name matching-border]))))]
         (if (seq matching-borders)
           (reduce
            (fn [res [o-1 o-2 matching-border]]
              (-> res
                  (assoc-in [(:id tile-1) o-1 (:id tile-2) o-2] matching-border)
                  (assoc-in [(:id tile-2) o-2 (:id tile-1) o-1] (reverse matching-border))))
            res
            matching-borders)
           res)))
     {}
     tile-combs)))

(defn get-corners
  [matching-tiles]
  (let [tile-id->neighbours (reduce-kv
                             (fn [res id-1 matches]
                               (let [neighbours (into #{}
                                                      (mapcat keys)
                                                      (vals matches))]
                                 (assoc res id-1 neighbours)))
                             {}
                             matching-tiles)
        corners (into []
                      (comp (filter #(= 2 (count (second %))))
                            (map first))
                      tile-id->neighbours)]
    corners))

(defn corner-to-corners-paths
  [edges corner-ids tile-id]
  (let [neighbours (get edges tile-id)
        corner-neightbours (filter (partial contains? corner-ids) neighbours)]
    (if (seq corner-neightbours)
      (mapv (partial vector tile-id) corner-neightbours)
      (let [neighbour-to-corners-paths (mapv (fn [neighbour]
                                               (let [corner-paths (corner-to-corners-paths edges corner-ids neighbour)]
                                                 (vec (mapcat #(into [tile-id] %) corner-paths))))
                                             neighbours)]
        neighbour-to-corners-paths))))

(defn get-neighbour-coords
  [grid-size [r c]]
  (let [n-coords [[[(dec r) c] [:bottom :top]]
                  [[r (dec c)] [:right :left]]
                  [[(inc r) c] [:top :bottom]]
                  [[r (inc c)] [:left :right]]]
        valid-n-coords (filterv
                        (fn [[[r c]]]
                          (and (not (neg? r))
                               (not (neg? c))
                               (< r grid-size)
                               (< c grid-size)))
                        n-coords)]
    valid-n-coords))

(defn fill-grid
  [grid grid-size matching-tiles]
  (reduce
   (fn [grid coord]
     (let [neighbour-coords (get-neighbour-coords grid-size coord)
           neighbours (reduce
                       (fn [res [n-coord matching-sides]]
                         (let [neighbour (get-in grid n-coord :not-found)]
                           (if neighbour
                             (conj res [neighbour matching-sides])
                             res)))
                       []
                       neighbour-coords)
           all-potential-tiles (into []
                                     (mapcat
                                      (fn [[neighbour matching-sides]]
                                        (let [n-id (:id neighbour)
                                              n-o (:orientation neighbour)
                                              n-matches (get-in matching-tiles [n-id n-o])
                                              possible-tiles (filterv
                                                              (fn [[_ m]]
                                                                (some #(= matching-sides
                                                                          (second %))
                                                                      m))
                                                              n-matches)]
                                          possible-tiles)))
                                     neighbours)]
       (cond
         (or (= 1 (count all-potential-tiles))
             (= 1 (count
                   (into #{}
                         (map #(vector (first %) (keys (second %))))
                         all-potential-tiles))))
         (let [[id m] (first all-potential-tiles)
               o (first (keys m))]
           (assoc-in grid coord {:id id :orientation o}))

         (empty? all-potential-tiles)
         (reduced (str "No Matching tiles for " coord))

         :else
         (reduced (str "Too many Matching tiles for " coord)))))
   grid
   (for [r (range grid-size)
         c (range grid-size)
         :when (not (and (zero? r)
                         (zero? c)))]
     [r c])))

(defn arrange-tiles
  [tiles]
  (let [matching-tiles (get-matching-tiles tiles)
        corners (get-corners matching-tiles)
        grid-size (long (Math/sqrt (count tiles)))]
    (for [corner corners]
      (let [corner-tile (get-in matching-tiles [corner :id] :not-found)
            corner-matching-sides (vec
                                   (sort
                                    (reduce
                                     (fn [res matches]
                                       (conj res (first (first (vals matches)))))
                                     []
                                     (vals corner-tile))))
            corner-orientation (case corner-matching-sides
                                 [:right :top] :fr
                                 [:left :top] :r180
                                 [:bottom :left] :fc
                                 [:bottom :right] :id)
            starting-grid (mapv (constantly (vec (repeat grid-size nil))) (range grid-size))
            starting-grid (assoc-in starting-grid [0 0]
                                    {:id corner :orientation corner-orientation})]
        (fill-grid starting-grid grid-size matching-tiles)))))

(defn arrange-image
  [tiles grid]
  (let [tile-size (-> tiles
                      first
                      :image
                      count
                      dec)]
    (reduce
     (fn [result row]
       (let [image-rows
             (reduce
              (fn [result m]
                (let [tile (util/first-match #(= (:id m) (:id %)) tiles)
                      tile (update tile :image (fn [image]
                                                 (subvec (mapv #(subvec % 1 tile-size) image)
                                                         1
                                                         tile-size)))
                      oriented-tile (case (:orientation m)
                                      :id tile
                                      :r90c (rotate-90-cw tile)
                                      :r90cc (rotate-90-ccw tile)
                                      :r180 (rotate-180 tile)
                                      :fr (flip-rows tile)
                                      :fc (flip-columns tile)
                                      :fr-r90c ((comp flip-rows rotate-90-cw) tile)
                                      :fr-r90cc ((comp flip-rows rotate-90-ccw) tile))
                      image (:image oriented-tile)]
                  (mapv #(into (get result %) (get image %)) (range (dec tile-size)))))
              (mapv (constantly []) (range (dec tile-size)))
              row)]
         (into result image-rows)))
     []
     grid)))

(defn challenge-1
  [tiles]
  (apply * (get-corners (get-matching-tiles tiles))))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [tiles]
  (let [arranged-tiles (first (arrange-tiles tiles))
        arranged-image (arrange-image tiles arranged-tiles)
        sea-monster  "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "
        sea-monster-num-hashes (get (frequencies sea-monster) \#)
        sea-monster-re (re-pattern (string/replace sea-monster " " "."))
        sea-monster (mapv seq (string/split-lines sea-monster))
        sea-monster-rs (count sea-monster)
        sea-monster-cs (count (first sea-monster))
        grid-size (long (Math/sqrt (count tiles)))
        tile-size (-> tiles
                      first
                      :image
                      count
                      (- 2))
        image-size (* grid-size tile-size)

        possible-orientations [identity
                               rotate-90-cw
                               rotate-90-ccw
                               rotate-180
                               flip-rows
                               flip-columns
                               (comp flip-rows rotate-90-cw)
                               (comp flip-rows rotate-90-ccw)]
        [sea-monster-count correct-image]
        (util/first-match
         #(not (zero? (first %)))
         (for [o possible-orientations
               :let [image (:image (o {:image arranged-image}))]]
           (reduce
            (fn [res [r c]]
              (let [sub-image (mapv #(subvec % c (+ c sea-monster-cs))
                                    (subvec image r (+ r sea-monster-rs)))
                    sub-image-str (string/join "\n" (mapv (partial string/join "") sub-image))]
                (if (re-matches sea-monster-re sub-image-str)
                  (update res 0 inc)
                  res)))
            [0 image]
            (for [r (range (- image-size sea-monster-rs))
                  c (range (- image-size sea-monster-cs))]
              [r c]))))]
    (- (get (frequencies (flatten correct-image)) \#)
       (* sea-monster-count sea-monster-num-hashes))))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))

(def input-string
  "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(def sample-input (parse-input input-string))
