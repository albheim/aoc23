(use sh)

(defn bench [day]
  (let [name (string/format "day%02d" day)
        file (string name ".janet")
        jimage (string "build/" name ".jimage")
        cmd (string "janet -i " jimage)]
    ($ janet -c ,file ,jimage)
    ($ hyperfine ,cmd)))

(defn main [& args]
  ($ mkdir -p build)
  (if (= 1 (length args))
    (do
      (var i 0)
      (while (file/open (string/format "day%02d.janet" (++ i)))
        (bench i)))
    (each idx (range 1 (length args))
      (bench (get args idx)))))
