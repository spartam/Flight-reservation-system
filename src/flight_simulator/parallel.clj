(ns flight-simulator.parallel
	(:require [clojure.string]
			  [clojure.pprint]
			  [flight-simulator.input-random :as input]
			  ;[flight-simulator.input-simple :as input]
	)
)

(def logger (agent nil))
(defn log [& msgs] (send logger (fn [_] (apply println msgs))))

(def flights
	"the flights are stored in a list"
	(atom []))

(def flights-per-carrier
	(atom []))

(def done? (atom false))

(defn init-flights [init-value]
	(reset! flights (map (fn [flight] (ref flight)) init-value))
	)

(defn init-flights-per-carrier [carriers]
	(reset! flights-per-carrier 
		(for [carrier input/carriers] 
			{ :carrier carrier
			  :flights (filter 
			  	(fn [flight] 
			  		(= (:carrier @flight) carrier)) 
			  	@flights)})))

(defn flights-of-carrier [carrier]
	(first (map :flights
		(filter (fn [carrierslist] 
					(= (:carrier carrierslist) carrier))
				@flights-per-carrier))))

(defn update-pricing [flight factor]
	(ref-set flight 
		(update @flight :pricing 
			#(map (fn [[p a t]] [(* p factor) a t]) %)))
	)

(defn update-carrier-price [carrier, factor]
	(let [carrierflights (flights-of-carrier carrier)]
		(dosync
			(doseq [flight carrierflights]
				   (update-pricing flight factor))
				))
	)

(defn start-sale [carrier]
	(log carrier "sale started")
	(update-carrier-price carrier 0.80))

(defn end-sale [carrier]
	(log carrier "sale ended")
	(update-carrier-price carrier 1.25))

(defn print-customer [{:keys [id from to seats budget]}]
	"Prints a single customer"
	(println (clojure.pprint/cl-format nil "Customer ~3d From ~a To ~a" id from to)))

(defn print-customers [customers]
	"Prints the customers (to test if the set was successfull)."
	(doseq [customer customers]
		(print-customer customer))
)

(defn sales-process []
  "The sales process starts and ends sales periods, until `finished-processing?`
  is true."
  (Thread/sleep input/TIME_BETWEEN_SALES)
  (loop []
    (let [discounted-carrier (rand-nth input/carriers)]
      (start-sale discounted-carrier)
      (Thread/sleep input/TIME_OF_SALES)
      (end-sale discounted-carrier))
      (Thread/sleep input/TIME_BETWEEN_SALES)
    (if (not @done?)
      (recur))))

(defn -main []
	(init-flights input/flights)
	(init-flights-per-carrier input/carriers)
	(let [sales-loop (future (sales-process))]

	(pmap print-customer input/customers) 
	(reset! done? true)
	@sales-loop
	(shutdown-agents)
))
