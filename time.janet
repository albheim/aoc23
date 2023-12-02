(use sh)

(defn main
  [& args]
  ($ mkdir -p build)
  (each idx (range 1 (length args))
    (do
      (let [file (string (get args idx) ".janet")
          jimage (string "build/" (get args idx) ".jimage")
          cmd (string "janet -i " jimage)]
      ($ janet -c ,file ,jimage)
      ($ hyperfine ,cmd)))))
