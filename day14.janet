(def grammar (peg/compile
  ~{:eol-eof (choice "\n" -1)
    :line (group (some (capture (set "#.O"))))
    :main (some (sequence :line :eol-eof))}))

(defn part1 [data]
  (def last-empty (array/new-filled (length (data 0)) 0))
  (var total 0)
  (each line-idx (range (length data))
    (each row-idx (range (length (data line-idx)))
      (match ((data line-idx) row-idx)
        "O" (do 
          (set total (+ total (- (length data) (last-empty row-idx))))
          (++ (last-empty row-idx)))
        "#" (put last-empty row-idx (inc line-idx))
        "." nil)))
  total)

(defn north [data]
  (def last-empty (array/new-filled (length (data 0)) 0))
  (each line-idx (range (length data))
    (each row-idx (range (length (data line-idx)))
      (match ((data line-idx) row-idx)
        "O" (do 
          (put (data line-idx) row-idx ".")
          (put (data (last-empty row-idx)) row-idx "O")
          (++ (last-empty row-idx)))
        "#" (put last-empty row-idx (inc line-idx))
        "." nil))))

(defn calc-weight [data]
  (var total 0)
  (each line-idx (range (length data))
    (each row-idx (range (length (data line-idx)))
      (if (= ((data line-idx) row-idx) "O")
        (set total (+ total (- (length data) line-idx))))))
  total)

(defn south [data]
  (def last-empty (array/new-filled (length (data 0)) (dec (length data))))
  (each line-idx (reverse (range (length data)))
    (each row-idx (range (length (data line-idx)))
      (match ((data line-idx) row-idx)
        "O" (do 
          (put (data line-idx) row-idx ".")
          (put (data (last-empty row-idx)) row-idx "O")
          (-- (last-empty row-idx)))
        "#" (put last-empty row-idx (dec line-idx))
        "." nil))))

(defn west [data]
  (each line-idx (range (length data))
    (var last-empty 0)
    (each row-idx (range (length (data line-idx)))
      (match ((data line-idx) row-idx)
        "O" (do 
          (put (data line-idx) row-idx ".")
          (put (data line-idx) last-empty "O")
          (++ last-empty))
        "#" (set last-empty (inc row-idx))
        "." nil))))

(defn east [data]
  (each line-idx (range (length data))
    (var last-empty (dec (length (data line-idx))))
    (each row-idx (reverse (range (length (data line-idx))))
      (match ((data line-idx) row-idx)
        "O" (do 
          (put (data line-idx) row-idx ".")
          (put (data line-idx) last-empty "O")
          (-- last-empty))
        "#" (set last-empty (dec row-idx))
        "." nil))))

(defn print-2d-array [arr]
  (print "Array: " (length arr) "x" (length (arr 0)))
  (each line arr
    (each item line
      (prin item " "))
    (print)))

(defn rotation [data]
  (north data)
  (west data)
  (south data)
  (east data))


(defn make-hashable [data] # TODO this probably takes a large chunk of the time
  (tuple ;(map |(tuple ;$) data)))

(defn part2 [data]
  (def visited @{(make-hashable data) 0})
  (var it 0)
  (def iterations 1000000000)
  (while (< it iterations)
    (++ it)
    (rotation data)
    (if-let [last-visit (get visited (make-hashable data))]
      (let [cycle (- it last-visit)]
        (set it (- iterations (mod (- iterations it) cycle)))) # TODO after this, set some bool to never need to call make-hashable again
      (put visited (make-hashable data) it)))
  (print (calc-weight data))
  (calc-weight data))

(defn main [& args]
  (let [data (peg/match grammar (slurp "data/day14.txt"))]
    (print "part1: " (part1 data))
    (print "part2: " (part2 data))))

# Testing, runs at compile time
(let [data (peg/match grammar 
`O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`)]
  (assert (= (part1 data) 136))
  (assert (= (part2 data) 64)))
