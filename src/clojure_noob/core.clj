(ns clojure-noob.core
  (:gen-class))
;; Sum To Target
;; main function: (sumToTarget [1 2 3 4] 10)
(defn checkAny [target] #(if (contains? %1 (- target %2))
	(reduced true)
	(conj %1 %2)))

(defn sumToTarget [input target] 
	(let [checkTarget (checkAny target)]
	(= true (reduce checkTarget #{} input))))

;; Print Spiral
;; main function: (printSpiral input)
(def input [["H" "A" "V"] ["D" "A" "E"] ["E" "Y" "A"] ["C" "I" "N"]])

(defn printExceptLast [inputArray layer]
	(subs (apply str inputArray) layer (- (count inputArray) (inc layer))))

(defn printTop [inputArray layer]
	(printExceptLast (inputArray layer) layer))

(defn printRight [inputArray layer]
	(printExceptLast (map #(% (- (count (inputArray 0)) (inc layer))) inputArray) layer))

(defn printBottom [inputArray layer]
	(printExceptLast (reverse (inputArray (- (count inputArray) (inc layer)))) layer))

(defn printLeft [inputArray layer]
	(printExceptLast (reverse (map #(% layer) inputArray)) layer))

(defn printLayer[inputArray layer]
	(str (printTop inputArray layer) (printRight inputArray layer) (printBottom inputArray layer) (printLeft inputArray layer)))

(defn printSpiral [input]
	(def width (count (input 0)))
	(def height (count input))
	(def totalLayers (inc (quot (min width height) 2)))
	(loop [layer 0]
		(print (printLayer input layer))
		(if (< layer (- totalLayers 1))
			(recur (inc layer))
			)))

;; Anagrams
;; main function: (anagram words)
(def words ["stop" "psto" "pots" "abc" "cba" "hello" "olleh" "hi"])

(defn sortString [hmap input]
	(let [sorted (sort (seq input))]
		(if (contains? hmap sorted)
			(assoc hmap sorted (str (get hmap sorted) " " input))
			(assoc hmap sorted input))))

(defn anagram [inputs]
	(let [hmap (reduce sortString {} inputs)]
		(map #(println (get hmap %)) (keys hmap))))