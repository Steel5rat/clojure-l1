(ns clojure-l1.core
	(:gen-class)
	(:require [clojure.java.io :as io])
	(:require [clojure.string :as str]))
;nth
(defn- calcPotentialFirstTime
	[inputString]
	inputString)


(defn- readFromFile
	[filePath]
(with-open [rdr (io/reader filePath)]
     (doall (map (fn [inputString] {:position (str/split inputString #",") :potential (calcPotentialFirstTime inputString)}) (line-seq rdr)))))
	

(defn -main 
  [& x]
  (print (readFromFile (nth x 0))))
