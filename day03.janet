(def grammar
  (peg/compile
    ~{:num (group (sequence (constant :isnum) (column) (line) (capture (some :d))))
      :symbol (group (sequence (column) (line) (if-not (choice :d "." "\n") (capture 1))))
      :main (sequence (any (choice :num :symbol "." "\n")) -1)}))

(defn parse-data [data]
  (let [m (peg/match grammar data)
        symbols @{}
        nums @[]]
    (each item m 
      (match item
        [:isnum x y val] (array/push nums [x y (length val) (scan-number val)])
        [x y sym] (put symbols [x y] sym)))
    [symbols nums]))

(defn has-surrounding-symbol [symbols x y len]
  (var found (or (get symbols [(- x 1) y]) (get symbols [(+ x len) y])))
  (for i (- x 1) (+ x len 1)
    (set found (or found (get symbols [i (- y 1)]) (get symbols [i (+ y 1)]))))
  found)

(defn part1 [symbols nums]
  (var tot 0)
  (each [x y len val] nums
    (if (has-surrounding-symbol symbols x y len)
      (set tot (+ tot val))))
  tot)

(defn find-gears [symbols stars [x y len val]]
  (defn check-star [pos]
    (if (= (get symbols pos) "*")
      (if (get stars pos)
        (array/push (get stars pos) val)
        (put stars pos @[val]))))
  (check-star [(- x 1) y])
  (check-star [(+ x len) y])
  (for i (- x 1) (+ x len 1)
    (check-star [i (- y 1)])
    (check-star [i (+ y 1)])))

(defn part2 [symbols nums]
  (def stars @{})
  (each num nums
    (find-gears symbols stars num))
  (sum (map |(product $) (filter (fn [x] (= (length x) 2)) (values stars)))))

(defn main [& args]
  (let [[symbols nums] (parse-data (slurp "data/day03.txt"))]
    (print "part1: " (part1 symbols nums))
    (print "part2: " (part2 symbols nums))))

# Testing, runs at compile time
(let [[symbols nums] (parse-data `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`)]
  (assert (= (part1 symbols nums) 4361))
  (assert (= (part2 symbols nums) 467835)))
