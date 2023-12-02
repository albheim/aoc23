(def grammar
  (peg/compile
    ~{:numcolor (group (sequence (number (any :d)) " " (capture (choice "red" "green" "blue"))))
      :draw (group (sequence (any (sequence :numcolor ", ")) :numcolor (at-most 1 "; ")))
      :main (sequence "Game " (number (any :d)) ": " (group (any :draw)))}))

(defn get-max-draw [draws]
  (var max-count @{"red" 0 "green" 0 "blue" 0})
  (each draw draws
    (each numcolor draw
      (update max-count (get numcolor 1) |(max $ (get numcolor 0)))))
  max-count)

(defn part1 [lines]
  (reduce 
    (fn [acc line]
      (let [m (peg/match grammar line)
            max-count (get-max-draw (get m 1))]
        (if (and  (<= (get max-count "red") 12) 
                  (<= (get max-count "green") 13) 
                  (<= (get max-count "blue") 14))
          (+ acc (get m 0))
          acc)))
    0 lines))

(defn part2 [lines]
  (reduce 
    (fn [acc line]
      (+ acc (product (values (get-max-draw (get (peg/match grammar line) 1))))))
    0 lines))

(defn main [& args]
  (let [lines (string/split "\n" (slurp "data/day02.txt"))]
    (print "part1: " (part1 lines))
    (print "part2: " (part2 lines))))


# Testing, runs at compile time
(let [lines (string/split "\n" 
  `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green`)]
  (assert (= (part1 lines) 8))
  (assert (= (part2 lines) 2286)))
