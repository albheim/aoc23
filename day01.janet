(def grammar-p1
  (peg/compile
    ~{:digit (replace (capture (range "09")) ,scan-number)
      :character (if-not (choice :digit) 1)
      :main (any (sequence (any :character) :digit))}))

(def grammar-p2
  (peg/compile
    ~{:digit (replace (capture (range "09")) ,scan-number)
      :one   (if "one"   (sequence 1 (constant 1)))
      :two   (if "two"   (sequence 1 (constant 2)))
      :three (if "three" (sequence 1 (constant 3)))
      :four  (if "four"  (sequence 1 (constant 4)))
      :five  (if "five"  (sequence 1 (constant 5)))
      :six   (if "six"   (sequence 1 (constant 6)))
      :seven (if "seven" (sequence 1 (constant 7)))
      :eight (if "eight" (sequence 1 (constant 8)))
      :nine  (if "nine"  (sequence 1 (constant 9)))
      :any-digit (choice :digit :one :two :three :four :five :six :seven :eight :nine)
      :character (if-not (choice :any-digit) 1)
      :main (any (sequence (any :character) :any-digit))}))

(defn solve [grammar lines]
  (sum (map |
    (let [m (peg/match grammar $)]
      (+ (* 10 (get m 0)) (get m (- (length m) 1))))
      lines)))

(defn main [& args]
  (let [lines (string/split "\n" (slurp "data/day01.txt"))]
    (print "part1: " (solve grammar-p1 lines))
    (print "part2: " (solve grammar-p2 lines))))


# Testing, runs at compile time

(let [test1 `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet`]
  (assert (= (solve grammar-p1 (string/split "\n" test1)) 142)))

(let [test2 `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen`]
  (assert (= (solve grammar-p2 (string/split "\n" test2)) 281)))
