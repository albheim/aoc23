(def grammar (peg/compile
  ~{:num (number (some :d))
    :num-line (group (sequence (any (sequence :num " ")) :num (choice "\n" -1)))
    :map (group (sequence (any :a) "-to-" (any :a) " map:\n" (some :num-line)))
    :main (sequence "seeds: " :num-line "\n" (some (sequence :map (opt "\n"))))}))

(defn part1 [[seeds & maps]]
  (var vals seeds)
  (each map-ranges maps
    (set vals (map (fn [val]
      (var new-val val)
      (each [map-dst map-src map-len] map-ranges
        (if (and (<= map-src val) (< val (+ map-src map-len)))
          (set new-val (+ map-dst (- val map-src)))))
      new-val)
    vals)))
  (min ;vals))
        
(defn map-val [val map-ranges]
  (var vals [val])
  (def converted-vals @[])
  (each [map-dst map-src map-len] map-ranges
    (def new-vals @[])
    (each [val-start val-len] vals
      (let [overlap-start (max val-start map-src)
            overlap-end (min (+ val-start val-len) (+ map-src map-len))]
        (if (< overlap-start overlap-end)
          (do 
            (array/push converted-vals [(+ (- overlap-start map-src) map-dst) (- overlap-end overlap-start)])
            (if (< overlap-end (+ val-start val-len))
              (array/push new-vals [overlap-end (- (+ val-start val-len) overlap-end)]))
            (if (< val-start overlap-start)
              (array/push new-vals [val-start (- overlap-start val-start)])))
          (array/push new-vals [val-start val-len])))
    (set vals new-vals)))
  [;vals ;converted-vals])
        
(defn part2 [[seeds & maps]]
  (var vals (map (fn [i] [(get seeds i) (get seeds (+ i 1))]) (range 0 (length seeds) 2)))
  (each map-ranges maps
    (set vals (mapcat |(map-val $ map-ranges) vals)))
  (min ;(map |(get $ 0) vals)))

(defn main [& args]
  (let [almanac (peg/match grammar (slurp "data/day05.txt"))]
    (print "part1: " (part1 almanac))
    (print "part2: " (part2 almanac))))

# Testing, runs at compile time
(let [almanac (peg/match grammar 
`seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4`)]
  (assert (= (part1 almanac) 35))
  (assert (= (part2 almanac) 46)))
