(ns cellular-automata.core
  (:require [clojure.string :as str])
  (:import [java.io BufferedOutputStream File FileOutputStream]
           [java.util Arrays]))

(defn die [msg]
  (.println System/err msg)
  (System/exit 1))

(defn parse-int [text]
  (let [t (str/trim text)]
    (when (re-matches #"-?\d+" t)
      (try
        (let [value (Long/parseLong t)]
          (when (and (>= value Integer/MIN_VALUE) (<= value Integer/MAX_VALUE))
            (int value)))
        (catch Exception _ nil)))))

(defn valid-seed [seed]
  (and (seq seed) (re-matches #"[01]+" seed)))

(defn -main [& _]
  (let [text (try
               (slurp "input.txt")
               (catch Exception _ (die "cannot read input.txt")))
        lines (str/split text #"\r?\n" -1)]
    (when (< (count lines) 3) (die "invalid input"))
    (let [rule (parse-int (nth lines 0))
          seed (nth lines 1)
          generations (parse-int (nth lines 2))]
      (when (or (nil? rule) (< rule 0) (> rule 255)) (die "invalid rule"))
      (when-not (valid-seed seed) (die "invalid initial conditions"))
      (when (or (nil? generations) (< generations 1)) (die "invalid generations"))
      (let [n (count seed)
            width (+ n (* 2 generations))
            even (zero? (bit-and rule 1))
            table (byte-array 8)
            prev (byte-array width)
            nxt (byte-array width)]
        (dotimes [i 8]
          (aset-byte table i (unchecked-byte (bit-and (unsigned-bit-shift-right rule i) 1))))
        (dotimes [i n]
          (when (= (nth seed i) \1)
            (aset-byte prev (+ generations i) (byte 1))))
        (.mkdirs (File. "results"))
        (let [path (str "results/r" rule "_g" generations "_i" seed "_clojure.pbm")
              line (byte-array (inc width))]
          (aset-byte line width (byte 10))
          (with-open [out (BufferedOutputStream. (FileOutputStream. path))]
            (.write out (.getBytes (str "P1\n" width " " generations "\n") "US-ASCII"))
            (loop [gen 0
                   prev prev
                   nxt nxt]
              (dotimes [j width]
                (aset-byte line j (unchecked-byte (if (zero? (aget prev j)) 48 49))))
              (.write out line)
              (when (< (inc gen) generations)
                (Arrays/fill nxt (byte 0))
                (let [lo (if even (- generations (inc gen)) 0)
                      hi (if even (+ generations n (inc gen)) width)]
                  (loop [j lo]
                    (when (< j hi)
                      (let [left (if (pos? j) (aget prev (dec j)) 0)
                            right (if (< (inc j) width) (aget prev (inc j)) 0)
                            center (aget prev j)
                            idx (bit-or (bit-shift-left (int left) 2)
                                        (bit-shift-left (int center) 1)
                                        (int right))]
                        (aset-byte nxt j (aget table idx)))
                      (recur (inc j)))))
                (recur (inc gen) nxt prev)))))))))

(-main)
