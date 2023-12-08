(def grammar (peg/compile
  ~{:node (capture 3)
    :instructions (sequence (group (some (capture (choice "L" "R")))) "\n")
    :connection (sequence :node " = (" (group (sequence :node ", " :node ")")) (choice "\n" -1))
    :main (sequence :instructions "\n" (replace (group (some :connection)) ,|(table ;$)))}))

(defn count-steps [instructions connections start done-fn]
  (var cnt -1)
  (var curr start)
  (while (not (done-fn curr))
    (set curr (get (get connections curr) 
                    (if (= (instructions (mod (++ cnt) (length instructions))) "L") 0 1))))
  (++ cnt))

(defn part1 [[instructions connections]]
  (count-steps instructions connections "AAA" |(= $ "ZZZ")))

(defn gcd [a b]
  (if (= b 0) a (gcd b (mod a b))))

(defn lcm [a b & rest]
  (if (empty? rest)
    (* a (/ b (gcd a b)))
    (lcm (lcm a b) ;rest)))
        
(defn part2 [[instructions connections]]
  (lcm ;(map |(count-steps instructions connections $ |(= (get $ 2) (chr "Z"))) 
          (filter |(= (get $ 2) (chr "A")) (keys connections)))))

(defn main [& args]
  (let [data (peg/match grammar (slurp "data/day08.txt"))]
    (print "part1: " (part1 data))
    (print "part2: " (part2 data))))

# Testing, runs at compile time
(let [data (peg/match grammar 
`RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)`)]
  (assert (= (part1 data) 2)))

(let [data (peg/match grammar 
`LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)`)]
  (assert (= (part2 data) 6)))
