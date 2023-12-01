(def grammar-p1
  ~{:digit (replace (capture (range "09")) ,scan-number)
    :character (if-not (choice :digit) 1)
    :main (sequence (any (sequence (any :character) :digit)) (any :character))})

(def grammar-p2
  ~{:digit (replace (capture (range "09")) ,scan-number)
    :one (if "one" (* 1 (constant 1)))
    :two (if "two" (* 1 (constant 2)))
    :three (if "three" (* 1 (constant 3)))
    :four (if "four" (* 1 (constant 4)))
    :five (if "five" (* 1 (constant 5)))
    :six (if "six" (* 1 (constant 6)))
    :seven (if "seven" (* 1 (constant 7)))
    :eight (if "eight" (* 1 (constant 8)))
    :nine (if "nine" (* 1 (constant 9)))
    :any-digit (choice :digit :one :two :three :four :five :six :seven :eight :nine)
    :character (if-not (choice :any-digit) 1)
    :main (sequence (any (sequence (any :character) :any-digit)) (any :character))})

(defn solve [grammar lines]
  (var tot 0)
  (each line lines
    (let [m (peg/match grammar line)]
      #(print line)
      #(pp m)
      (set tot (+ tot (* 10 (get m 0)) (get m (- (length m) 1))))))
  tot)

(defn part1 [lines]
  (solve grammar-p1 lines))

(let [data `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet`]
  (print "test1: " (part1 (string/split "\n" data))))

(defn part2 [lines]
  (solve grammar-p2 lines))

(let [data `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen`]
  (print "test2: " (part2 (string/split "\n" data))))

(defn main [& args]
  (let [lines (string/split "\n" (slurp "data/day01.txt"))]
    (print "part1: " (part1 lines))
    (print "part2: " (part2 lines))))
