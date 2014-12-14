(ns clojure-l1.core
	(:gen-class)
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as str])
	)
;nth
(def ra 3)
(def rb 4.5);1.5 ra
(def epsilonUp 0.5)
(def epsilonDown 0.15)

(defn calcDistance
	[itemI itemJ]
	(
		apply + (map (fn [propertyI propertyJ] (if (= propertyI propertyJ) 0 1)) itemI itemJ)
	)
)

;(defn- -main
;	[& x]
;	(
;		 print ( reduce + (map (fn [propertyI, propertyJ] (if (= propertyI propertyJ) 0 1)) (nth x 0) (nth x 1)));hamming
;	)
;)

(defn calcPotentialFirstTime
	[itemI collection]
	(
		apply + (map (fn [itemJ] ( Math/exp(- (* (calcDistance (:position itemI) (:position itemJ)) (/ 4 (* ra ra)))))) collection  )  ;(/ 4 (* ra ra)) = alpha
	)
)

(defn calcPotentialAnyTime
	[item centers]
	(let [center (first centers)]
		(
			- (:potential item) (* (:potential center) (Math/exp (- (* (calcDistance (:position item) (:position center)) (/ 4 (* rb rb))))))
		)
	)
)

(defn- readFromFile
	[filePath]
	(map (fn [rangeId inputItem] {:position (drop-last 1 (:position inputItem)) :potential 0 :id rangeId})	
		 (range 1 (java.lang.Integer/MAX_VALUE))
		 (with-open [rdr (io/reader filePath)]
		     (doall (map (fn [inputString] {:position (str/split inputString #",") :potential 0}) (line-seq rdr)))
		)
	)
)
	
(defn getMinimalDistance
	[newCenter centers]
	( apply min (map (fn [item] (calcDistance newCenter item)) centers))
)

(defn checkRecFin
	[collection centers newCenter]
	(if (> (:potential (first centers)) (* epsilonUp (:potential (last centers)) ))
		(cons newCenter centers)
		(if(< (:potential (first centers)) (* epsilonDown (:potential (last centers)) ))
			centers
			(if(>= (+ (/ (getMinimalDistance newCenter centers) ra) (/ (:potential newCenter) (:potential (last centers)))) 1)
				(cons newCenter centers)
				{:centers centers}
			)
		)
	)
)

(defn recursionBody
	[collection centers]
		(let [inputCollection (map (fn [item] {:position (:position item) :potential (calcPotentialAnyTime item centers) :id (:id item)}) collection)]
			(let [newCenters ( checkRecFin inputCollection centers (apply max-key (fn [item] (:potential item)) inputCollection) )]
				(if (= newCenters centers)
					newCenters
					(recursionBody inputCollection newCenters)
				))
		)
)


(defn -main
  [& x]
   (
   		doseq
   		[center (
   			let [collectionWithPotentials
	   			(let [inputCollection (readFromFile (nth x 0))]
				(
					map (fn [item] {:position (:position item) :potential (calcPotentialFirstTime item inputCollection) :id (:id item)}) inputCollection
				))]
			(
				map (fn [item] {:String (:id item) :Position (:position item) }) (recursionBody collectionWithPotentials [(apply max-key (fn [item] (:potential item)) collectionWithPotentials)])
			)
  		)]
  		(println center)
   )
)
