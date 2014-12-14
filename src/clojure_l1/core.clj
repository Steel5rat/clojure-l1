(ns clojure-l1.core
	(:gen-class)
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as str])
	)
;nth
(def ra 4)
(def rb 6)
(def epsilonUp 0.5)
(def epsilonDown 0.15)

(defn calcDistance
	[pointI pointJ]
	(
		let [itemI (:position pointI) itemJ (:position pointJ)](
		if (= (:distance pointJ) "Hamming")
			(apply + (map (fn [propertyI propertyJ] (if (= propertyI propertyJ) 0 1)) itemI itemJ))
			(if(= (:distance pointJ) "Euclid")
				(Math/sqrt (apply + (map (fn [propertyI propertyJ] (Math/pow (- (read-string propertyI) (read-string  propertyJ)) 2)) itemI itemJ))))
		)
	)
)

(defn calcPotentialFirstTime
	[itemI collection]
	(
		apply + (map (fn [itemJ] ( Math/exp(- (* (calcDistance itemI itemJ) (/ 4 (* ra ra)))))) collection  )  ;(/ 4 (* ra ra)) = alpha
	)
)

(defn calcPotentialAnyTime
	[item centers]
	(let [center (first centers)]
		(
			- (:potential item) (* (:potential center) (Math/exp (- (* (calcDistance item center) (/ 4 (* rb rb))))))
		)
	)
)

(defn- readFromFile
	[filePath distanceMethod]
	(map (fn [rangeId inputItem] {:position (drop-last 1 (:position inputItem)) :potential 0 :id rangeId :distance distanceMethod})	
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
	(if (> (:potential (first centers)) (* epsilonUp (:potential (last centers))))
		{:centers (cons newCenter centers) :collection collection}
		(if(< (:potential (first centers)) (* epsilonDown (:potential (last centers)) ))
			{:centers centers}
			(if(>= (+ (/ (getMinimalDistance newCenter centers) ra) (/ (:potential newCenter) (:potential (last centers)))) 1)
				{:centers (cons newCenter centers) :collection collection}
				(let [newCollection (map (fn [item] {:position (:position item) :potential (if(= (:id newCenter) (:id item)) 0 (:potential item)) :id (:id item) :distance (:distance item)}) collection )]
					{:centers (cons (apply max-key (fn [item] (:potential item)) newCollection) centers) :collection newCollection}
				)
			)
		)
	)
)

(defn recursionBody
	[collection centers]
		(let [inputCollection (map (fn [item] {:position (:position item) :potential (calcPotentialAnyTime item centers) :id (:id item) :distance (:distance item)}) collection)]
			(let [newData ( checkRecFin inputCollection centers (apply max-key (fn [item] (:potential item)) inputCollection) )]
				(if (nil? (:collection newData))
					(:centers newData)
					(recursionBody (:collection newData) (:centers newData))
				))
		)
)


(defn -main
  [& x]
   (
   		doseq
   		[centers (
   			let [collectionWithPotentials
	   			(let [inputCollection (readFromFile (nth x 0) (nth x 1))]
				(
					map (fn [item] {:position (:position item) :potential (calcPotentialFirstTime item inputCollection) :id (:id item) :distance (:distance item)}) inputCollection
				))]
			(
				map (fn [item] {:String (:id item) :Position (:position item) }) (recursionBody collectionWithPotentials [(apply max-key (fn [item] (:potential item)) collectionWithPotentials)])
			)
  		)]
  		(println centers)
   )
)
