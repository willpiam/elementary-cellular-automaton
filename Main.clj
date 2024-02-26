(ns cellular-automata.core
  (:require [clojure.string :as str])
  (:import [java.time Instant]))

(defn calculate-cell [p-state rule]
  (case p-state
    "111" (str (first rule))
    "110" (str (nth rule 1))
    "101" (str (nth rule 2))
    "100" (str (nth rule 3))
    "011" (str (nth rule 4))
    "010" (str (nth rule 5))
    "001" (str (nth rule 6))
    "000" (str (nth rule 7))))

(defn pad-zeros [rval target-depth]
  (if (or (< target-depth 1) (= (count rval) target-depth))
    rval
    (recur (str rval "0") target-depth)))

(defn binary-string [x]
  (let [bs (Integer/toBinaryString x)]
    (if (>= (count bs) 8)
      bs
      (str (pad-zeros "" (- 8 (count bs))) bs))))

(defn ensure-length-three [s making]
  (cond
    (= (count s) 3) s
    (empty? making) (str "0" s)
    :else (str s (pad-zeros "" (- 3 (count s))))))

(defn pad-gen [gen pad-to]
  (let [missing-zeros (- pad-to (quot (count gen) 2))
        zeros (pad-zeros "" missing-zeros)]
    (str zeros gen zeros)))

(defn safe-substring [s start end]
  (let [start (max 0 start)
        end (min (count s) end)]
    (subs s start end)))

(defn generate-line [prev rule making limit initial-condition-length]
  (if (= (count making) (count prev))
    (pad-gen making (dec (+ limit initial-condition-length)))
    (let [substr (safe-substring prev (dec (count making)) (+ 2 (count making)))
          psubstr (ensure-length-three substr making)
          newmaking (str making (calculate-cell psubstr rule))]
      (recur prev rule newmaking limit initial-condition-length))))

(defn generate [prev rule count limit state initial-condition-length]
  (if (>= count limit)
    state
    (let [this-line (generate-line prev rule "" limit initial-condition-length)
          new-state (str state "\n" this-line)]
      (recur this-line rule (inc count) limit new-state initial-condition-length))))

(defn parse-number-with-default [def s]
  (if (empty? s) def (Long/parseLong s)))

(defn initial-conditions-or-default [condition]
  (if (empty? condition) "010" condition))

(defn -main [& args]
  (let [contents (slurp "input.txt")
        [s-rule incon slines] (str/split-lines contents)
        rule (parse-number-with-default 30 s-rule)
        temp-initial-conditions (initial-conditions-or-default incon)
        initial-length (count temp-initial-conditions)
        nlines (parse-number-with-default 12 slines)
        initial-conditions (pad-gen temp-initial-conditions (+ nlines initial-length -1))
        lines (generate initial-conditions (binary-string rule) 0 (dec nlines) initial-conditions initial-length)
        fprefix (str "results/r" rule "_g" slines "_i" temp-initial-conditions "_clojure")
        pbm-text (str "P1\n" (count initial-conditions) " " nlines "\n" lines "\n")]
    (spit (str fprefix ".pbm") pbm-text)
    ))

(-main)
