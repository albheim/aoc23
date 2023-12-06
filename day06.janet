(def grammar-p1 (peg/compile
  ~{:num (number (some :d))
    :num-line (group (any (sequence (any " ") :num)))
    :main (sequence "Time:" :num-line "\nDistance:" :num-line -1)}))

(def grammar-p2 (peg/compile
  ~{:num (replace (accumulate (any (sequence (any " ") (capture (some :d))))) ,scan-number)
    :main (sequence "Time:" :num "\nDistance:" :num -1)}))

(defn solve-race [time distance]
  (def t (+ (math/floor (/ (- time (math/sqrt (- (* time time) (* 4 distance)))) 2)) 1))
  (+ (- time (* 2 t)) 1))

(defn part1 [[times distances]]
  (product (map |(solve-race (get times $) (get distances $)) (range (length times)))))
        
(defn part2 [[time distance]]
  (solve-race time distance))

(defn main [& args]
  (let [input (slurp "data/day06.txt")]
    (print "part1: " (part1 (peg/match grammar-p1 input)))
    (print "part2: " (part2 (peg/match grammar-p2 input)))))

# Testing, runs at compile time
(let [input 
`Time:      7  15   30
Distance:  9  40  200`]
  (assert (= (part1 (peg/match grammar-p1 input)) 288))
  (assert (= (part2 (peg/match grammar-p2 input)) 71503)))
