(def grammar (peg/compile
  ~{:num (number (some (choice "-" :d)))
    :num-line (group (sequence (any (sequence :num " ")) :num))
    :main (some (sequence :num-line (choice "\n" -1)))}))

(defn diff [data]
  (def new-data (array/new (- (length data) 1)))
  (each idx (range 0 (- (length data) 1))
    (put new-data idx (- (get data (+ idx 1)) (get data idx))))
  new-data)

(defn what-is-next [data]
  (if (all |(= $ 0) data)
    0
    (+ (last data) (what-is-next (diff data)))))

(defn what-is-prev [data]
  (if (all |(= $ 0) data)
    0
    (- (first data) (what-is-prev (diff data)))))
        
(defn solve [data map-fn]
  (reduce (fn [acc sequence] 
            (+ acc (map-fn sequence)))
    0 data))

(defn main [& args]
  (let [data (peg/match grammar (slurp "data/day09.txt"))]
    (print "part1: " (solve data what-is-next))
    (print "part2: " (solve data what-is-prev))))

# Testing, runs at compile time
(let [data (peg/match grammar 
`0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45`)]
  (assert (= (solve data what-is-next) 114))
  (assert (= (solve data what-is-prev) 2)))
