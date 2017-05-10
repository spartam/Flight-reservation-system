(ns flight-simulator.parallel
	(:require [clojure.string]
			  [clojure.pprint]
			  [flight-simulator.input-random :as input]
			  ;[flight-simulator.input-simple :as input]
	)
)

(def customers
	"All customers are kept in a ref. This enables different threads to take a customer
	from the list and update the list until all customers have been processed."
	(ref []))

(def done? (atom false))

(defn initialize-customers [init-customers]
	(dosync 
		(ref-set customers init-customers)
	)
)

(defn take-customer []
	(dosync
		(let [f (first @customers)
			 r (rest @customers)
			 len (count r)]
			 (ref-set customers r)

			 (if (empty? r)
			 	(reset! done? true))
			 f)
	)
)

(defn print-customer [{:keys [id from to seats budget]}]
	"Prints a single customer"
	(println (clojure.pprint/cl-format nil "Customer ~3d From ~a To ~a" id from to)))

(defn print-customers [customers]
	"Prints the customers (to test if the set was successfull)."
	(doseq [customer customers]
		(print-customer customer))
)

(defn process-customers []
	(let [customer (take-customer)]
		(print-customer customer)
		(if (not @done?)
			(recur))))

(defn split-in-thread []
	(let [f1 (future (process-customers))
		  f2 (future (process-customers))
		  f3 (future (process-customers))
		  f4 (future (process-customers))]	
	@f1
	@f2
	@f3
	@f4
	)
)

(defn main []
	(initialize-customers input/customers)
	(let [f1 (future (time (split-in-thread)))]	
	@f1
	)
)

(main)
(shutdown-agents)