(def grammar (peg/compile
  ~{:num (number (some :d))
    :line (group (sequence (group (any (capture (if-not " " 1)))) " " :num (choice "\n" -1)))
    :main (any :line)}))

(defn hand-value [hand-counts]
  (match hand-counts
    [5 & _]   [5 0]
    [4 & _]   [4 0]
    [3 2 & _] [3 2]
    [3 & _]   [3 0]
    [2 2 & _] [2 2]
    [2 & _]   [2 0]
    _         [1 0]))

(defn count-cards [hand]
  (var counts @{})
  (each c hand
    (put counts c (+ 1 (get counts c 0))))
  counts)

(defn score-1 [hand]
  (def card-values {"2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "T" 10 "J" 11 "Q" 12 "K" 13 "A" 14})
  [;(hand-value (sort (values (count-cards hand)) >)) ;(map |(get card-values $) hand)])

(defn score-2 [hand]
  (def card-values {"2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "T" 10 "J" 1 "Q" 12 "K" 13 "A" 14})
  (def counts (count-cards hand))
  (def jacks (get counts "J" 0))
  (put counts "J" 0)
  (def sorted-counts (sort (values counts) >))
  (update sorted-counts 0 |(+ $ jacks))
  [;(hand-value sorted-counts) ;(map |(get card-values $) hand)])

(defn solve [data score-fn]
  (let [scored-hands (map (fn [[hand val]] [(score-fn hand) val]) data)
        sorted-hands (sort scored-hands)]
    (reduce (fn [acc i] 
        (+ acc (* (+ i 1) (get (get sorted-hands i) 1)))) 
      0 
      (range (length sorted-hands)))))

(defn main [& args]
  (let [data (peg/match grammar (slurp "data/day07.txt"))]
    (print "part1: " (solve data score-1))
    (print "part2: " (solve data score-2))))

# Testing, runs at compile time
(let [data (peg/match grammar 
`32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483`)]
  (assert (= (solve data score-1) 6440))
  (assert (= (solve data score-2) 5905)))
