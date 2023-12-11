(def grammar (peg/compile
  ~{:line (sequence (group (any (capture (choice "." "#")))) (choice "\n" -1))
    :main (some :line)}))

(defn solve [grid expansion]
  (def empty-cols (seq [col :range [0 (length (grid 0))]]
                    (if (all |(= ((grid $) col) ".") (range (length grid)))
                      true
                      false)))
  (def empty-rows (seq [row :range [0 (length grid)]]
                    (if (all |(= ((grid row) $) ".") (range (length (grid row))))
                      true
                      false)))
  (def galaxies (seq [row :range [0 (length grid)]
                      col :range [0 (length (grid 0))]
                      :when (= ((grid row) col) "#")]
                  [row col]))
  (var total-length 0)
  (loop [[row-1 col-1] :in galaxies
         [row-2 col-2] :in galaxies
         :when (or (< row-1 row-2) (and (= row-1 row-2) (< col-1 col-2)))] 
    (set total-length (- total-length 2))
    (loop [row :range-to [(min row-1 row-2) (max row-1 row-2)]]
      (set total-length (+ total-length
                          (if (empty-rows row)
                            expansion 1))))
    (loop [col :range-to [(min col-1 col-2) (max col-1 col-2)]]
      (set total-length (+ total-length
                          (if (empty-cols col)
                            expansion 1)))))
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
