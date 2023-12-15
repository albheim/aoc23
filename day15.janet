(def grammar (peg/compile
  ~{:end (choice "," -1)
    :step (capture (some (if-not :end 1)))
    :main (some (sequence :step :end))}))

(defn my-hash [s]
  (var curr 0)
  (each c s
    (set curr (mod (* 17 (+ curr c)) 256)))
  curr)

(defn part1 [data]
  (sum (map |(my-hash $) data)))
        
(def grammar-step (peg/compile
  ~{:add (sequence (capture (some :a)) "=" (number :d))
    :remove (sequence (capture (some :a)) "-")
    :main (choice :add :remove)}))

(defn part2 [data]
  (def boxes (seq [i :range [0 256]] @{}))
  (each idx (range (length data))
    (let [text (data idx)
          instruction (peg/match grammar-step text)
          box-idx (my-hash (first instruction))]
      (match instruction
        [lab focal] (match (get (boxes box-idx) lab)
                    nil (put (boxes box-idx) lab @[idx focal])
                    [old-idx _] (put (boxes box-idx) lab @[old-idx focal]))
        [lab] (put (boxes box-idx) lab nil))))
  (var focusing-power 0)
  (each box-idx (range (length boxes))
    (let [focals (map |(get $ 1) (sort (values (boxes box-idx)) (fn [a b] (< (first a) (first b)))))]
      (each slot-idx (range (length focals))
        (set focusing-power (+ focusing-power (* (inc box-idx) (inc slot-idx) (focals slot-idx)))))))
  focusing-power)

(defn main [& args]
  (let [data (peg/match grammar (slurp "data/day15.txt"))]
    (print "part1: " (part1 data))
    (print "part2: " (part2 data))))

# Testing, runs at compile time
(let [data (peg/match grammar `rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7`)]
  (assert (= (part1 data) 1320))
  (assert (= (part2 data) 145)))
