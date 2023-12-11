(def grammar (peg/compile
  ~{:line (sequence (group (any (capture (choice "." "#")))) (choice "\n" -1))
    :main (some :line)}))

(defn cumsum [xs]
  (def result (array/new (length xs)))
  (var acc (xs 0))
  (loop [idx :range [0 (length xs)]]
    (put result idx acc)
    (set acc (+ acc (xs idx))))
  result)

(defn solve [grid expansion]
  (def prefix-dist-cols (cumsum (seq [col :range [0 (length (grid 0))]]
                    (if (all |(= ((grid $) col) ".") (range (length grid)))
                      expansion
                      1))))
  (def prefix-dist-rows (cumsum (seq [row :range [0 (length grid)]]
                    (if (all |(= ((grid row) $) ".") (range (length (grid row))))
                      expansion 
                      1))))
  (def galaxies (seq [row :range [0 (length grid)]
                      col :range [0 (length (grid 0))]
                      :when (= ((grid row) col) "#")]
                  [row col]))
  (var total-length 0)
  (loop [[row-1 col-1] :in galaxies
         [row-2 col-2] :in galaxies
         :when (or (< row-1 row-2) (and (= row-1 row-2) (< col-1 col-2)))] 
    (set total-length (+
      total-length
      (math/abs (- (prefix-dist-rows row-2) (prefix-dist-rows row-1)))
      (math/abs (- (prefix-dist-cols col-2) (prefix-dist-cols col-1))))))
  total-length)

(defn main [& args]
  (let [grid (peg/match grammar (slurp "data/day11.txt"))]
    (print "part1: " (solve grid 2))
    (print "part2: " (solve grid 1000000))))

# Testing, runs at compile time
(let [grid (peg/match grammar 
`...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....`)]
  (assert (= (solve grid 2) 374))
  (assert (= (solve grid 10) 1030)))
