(def grammar
  (peg/compile
    ~{:num (number (some :d))
      :nums (any (sequence (any " ") :num))
      :main (any (group (sequence "Card" (any " ") :num ":" (group :nums) (any " ") "|" (group :nums) (choice "\n" -1))))}))

(defn part1 [cards]
  (var tot 0)
  (each card cards
    (let [winning (zipcoll (get card 1) (get card 1))
          owned (get card 2)]
      (var wins 0.5)
      (each num owned 
        (if (has-key? winning num)
          (set wins (* wins 2))))
      (if (>= wins 1)
        (set tot (+ tot wins)))))
  tot)
        
(defn part2 [cards]
  (def card-counts (map (fn [x] 1) cards))
  (each idx (range (length cards))
    (let [card (get cards idx)
          winning (zipcoll (get card 1) (get card 1))
          owned (get card 2)]
      (def matches (count (fn [num] (has-key? winning num)) owned))
      (each idx2 (range (+ idx 1) (+ idx matches 1))
        (put card-counts idx2 (+ (get card-counts idx2) (get card-counts idx))))))
  (sum card-counts))

(defn main [& args]
  (let [cards (peg/match grammar (slurp "data/day04.txt"))]
    (print "part1: " (part1 cards))
    (print "part2: " (part2 cards))))

# Testing, runs at compile time
(let [cards (peg/match grammar 
`Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`)]
  (assert (= (part1 cards) 13))
  (assert (= (part2 cards) 30)))
