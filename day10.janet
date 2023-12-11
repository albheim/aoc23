(def grammar (peg/compile
  ~{:eol-eof (choice "\n" -1)
    :line (group (sequence (any (capture (if-not :eol-eof 1))) :eol-eof))
    :main (any :line)}))

(defn find-start-and-neighbours [grid]
  (var start 0)
  (loop [i :range [0 (length grid)]
         j :range [0 (length (grid 0))]
         :when (= ((grid i) j) "S")]
    (set start [i j]))
  (def [y x] start)
  (def neighbours @[])
  (if (> x 0)
    (let [west ((grid y) (- x 1))]
      (if (or (= west "-") (= west "F") (= west "L"))
        (array/push neighbours [[y (- x 1)] "E"]))))
  (if (< x (- (length (grid 0)) 1))
    (let [east ((grid y) (+ x 1))]
      (if (or (= east "-") (= east "7") (= east "J"))
        (array/push neighbours [[y (+ x 1)] "W"]))))
  (if (> y 0)
    (let [north ((grid (- y 1)) x)]
      (if (or (= north "|") (= north "F") (= north "7"))
        (array/push neighbours [[(- y 1) x] "S"]))))
  (if (< y (- (length grid) 1))
    (let [south ((grid (+ y 1)) x)]
      (if (or (= south "|") (= south "J") (= south "L"))
        (array/push neighbours [[(+ y 1) x] "N"]))))
  [start neighbours])

(defn step [grid [[y x] orig]]
  (let [pipe ((grid y) x)]
    (match [orig pipe]
      ["S" "F"] [[y (+ x 1)] "W"]
      ["S" "|"] [[(- y 1) x] "S"]
      ["S" "7"] [[y (- x 1)] "E"]
      ["N" "L"] [[y (+ x 1)] "W"]
      ["N" "|"] [[(+ y 1) x] "N"]
      ["N" "J"] [[y (- x 1)] "E"]
      ["E" "F"] [[(+ y 1) x] "N"]
      ["E" "-"] [[y (- x 1)] "E"]
      ["E" "L"] [[(- y 1) x] "S"]
      ["W" "7"] [[(+ y 1) x] "N"]
      ["W" "-"] [[y (+ x 1)] "W"]
      ["W" "J"] [[(- y 1) x] "S"])))


(defn part1 [grid start neighbours]
  (var state-1 (neighbours 0))
  (var state-2 (neighbours 1))
  (var cnt 1)
  (while (not (= (get state-1 0) (get state-2 0)))
    (set state-1 (step grid state-1))
    (set state-2 (step grid state-2))
    (++ cnt))
  cnt)

(defn color-some [grid coloring color & positions]
  (each [y x] positions
    (if (and (>= x 0) (>= y 0) (< x (length (grid 0))) (< y (length grid)))
      (if (not (= (get coloring [y x]) 3))
        (put coloring [y x] color))
      (put coloring [-1 -1] color)))) # Fix special case by saving outside color in -1 -1

(defn color-state [grid [[y x] orig] coloring]
  (color-some grid coloring 3 [y x])
  (match ((grid y) x)
    "F" (let [color (if (= orig "S") 1 2)]
          (color-some grid coloring color [y (- x 1)] [(- y 1) (- x 1)] [(- y 1) x]))
    "|" (let [color (if (= orig "S") 1 2)]
          (color-some grid coloring color [y (- x 1)])
          (color-some grid coloring (- 3 color) [y (+ x 1)]))
    "7" (let [color (if (= orig "W") 1 2)]
          (color-some grid coloring color [(- y 1) x] [(- y 1) (+ x 1)] [y (+ x 1)]))
    "L" (let [color (if (= orig "E") 1 2)]
          (color-some grid coloring color [(+ y 1) x] [(+ y 1) (- x 1)] [y (- x 1)]))
    "J" (let [color (if (= orig "N") 1 2)]
          (color-some grid coloring color [(+ y 1) x] [(+ y 1) (+ x 1)] [y (+ x 1)]))
    "-" (let [color (if (= orig "E") 1 2)]
          (color-some grid coloring color [(+ y 1) x])
          (color-some grid coloring (- 3 color) [(- y 1) x]))))

(defn fill-if-zero [grid coloring [y x]]
  (if (or (< x 0) (< y 0) (>= x (length (grid 0))) (>= y (length grid)))
    nil
    (match (get coloring [y x] 0)
      3 nil
      -1 nil
      0 (do 
            (put coloring [y x] -1)
            (if-let [col2 (fill-if-zero grid coloring [y (- x 1)])]
              (put coloring [y x] col2))
            (if-let [col2 (fill-if-zero grid coloring [(+ y 1) x])]
              (put coloring [y x] col2))
            (if-let [col2 (fill-if-zero grid coloring [(- y 1) x])]
              (put coloring [y x] col2))
            (if-let [col2 (fill-if-zero grid coloring [y (+ x 1)])]
              (put coloring [y x] col2))
            (get coloring [y x]))
      c (do (put coloring [y x] c) c))))

(defn part2 [grid start neighbours]
  (var state (neighbours 0))
  (def coloring @{start 3}) # 0 not visited, 1 on the left, 2 on the right, 3 part of loop
  (color-state grid state coloring)
  (while (not (= start (get state 0)))
    (color-state grid state coloring)
    (set state (step grid state)))

  # From each zero, search along zeros until you find a 1 or 2.
  (loop [i :range [0 (length grid)]
         j :range [0 (length (grid 0))]]
    (if (= (get coloring [i j] 0) 0)
      (do 
        (put coloring [i j] 0)
        (fill-if-zero grid coloring [i j]))))

  (var cnt-color-1 0)
  (var cnt-color-2 0)
  (loop [i :range [0 (length grid)]
         j :range [0 (length (grid 0))]]
    (if (= (get coloring [i j]) 1)
      (++ cnt-color-1)
      (if (= (get coloring [i j]) 2)
        (++ cnt-color-2))))
    
  (var outside-color 0)
  (loop [i :range [0 (length grid)]
         j :range [0 (length (grid 0))]
         :when (or (= i 0) (= j 0) (= i (- (length grid) 1)) (= j (- (length (grid 0)) 1)))]
    (if (not (= (get coloring [i j]) 3))
      (set outside-color (get coloring [i j]))))
  (if (= outside-color 0)
    (set outside-color (get coloring [-1 -1])))
  (if (= outside-color 1)
    cnt-color-2
    cnt-color-1))
  

(defn main [& args]
  (let [grid (peg/match grammar (slurp "data/day10.txt"))
        [start neighbours] (find-start-and-neighbours grid)]
    (print "part1: " (part1 grid start neighbours))
    (print "part2: " (part2 grid start neighbours))))

# Testing, runs at compile time
(let [grid (peg/match grammar 
`.....
.S-7.
.|.|.
.L-J.
.....`)
      [start neighbours] (find-start-and-neighbours grid)]
  (assert (= (part1 grid start neighbours) 4))
  (assert (= (part2 grid start neighbours) 1)))

(let [grid (peg/match grammar 
`FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L`)
      [start neighbours] (find-start-and-neighbours grid)]
  (assert (= (part2 grid start neighbours) 10)))
