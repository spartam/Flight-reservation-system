(ns flight-simulator.parallel
	(:require [clojure.string]
		[clojure.pprint]
		[flight-simulator.input-random :as input]
		;[flight-simulator.input-simple :as input]
		)
	)

(def logger (agent nil))
;(defn log [& msgs] (send logger (fn [_] (apply println msgs))))
(defn log [& msgs] true)

(def flights
	"the flights are stored in a list"
	(atom []))

(def flights-per-carrier
	(atom []))

(def recurcount
	(atom 0))

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
	"updates the price of a given flight(ref) since we're updating the ref,
	it must be called within a dosync procedure"
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


(defn print-flights [flights]
  "Print `flights`."
  (letfn [(pricing->str [pricing]
            (->> pricing
              (map (fn [[p a t]] (clojure.pprint/cl-format nil "$~3d: ~3d ~3d" p a t)))
              (clojure.string/join ", ")))]
    (doseq [flight flights]
    	(let [{:keys [id from to pricing]} @flight]
    	      (println (clojure.pprint/cl-format nil "Flight ~3d from ~a to ~a: ~a"
    	        id from to (pricing->str pricing)))))))


(defn buy-seats [flight, seats, price]
	(dosync
		(let [result (filter (fn [[p a t]] (and (= p price) (>= a seats))) (:pricing @flight))]
		(if (not (empty? result)) 
			(do
				(ref-set flight
					(update @flight :pricing
						#(map (fn [[p a t]] 
							(if (= p price) 
								[p (- a seats) (+ t seats)]
								[p a t])) %)))
				true)
			false))
		)
	)

(defn find-flights-with-n-seats-and-within-budget [from to seats budget] 
	(filter (fn [flight-ref]
		(and
			(= (:from @flight-ref) from)
			(= (:to @flight-ref) to)
			(not (nil? 
				(some (fn [[p a t]]
					(and (< p budget)
						(>= a seats))) (:pricing @flight-ref))
				)
			))) @flights)
	)

(defn cheapest-available-price [flight budget seats]
	(loop [prices (sort-by first (:pricing @flight))]
		(if (empty? prices) nil
		(let [current (first prices)
			  currentprice (first current)
			  currentavailable (nth current 1)]
			  (if (and (< currentprice budget)
			  		   (>= currentavailable seats))
			  currentprice
			  (recur (rest prices))))
		)))

(defn find-cheapest-flight-with-n-seats-and-within-budget [from to seats budget]
	(let [flights (sort-by (fn [flight] (cheapest-available-price flight budget seats)) (find-flights-with-n-seats-and-within-budget from to seats budget))]
		(if (empty? flights)
			nil
			(first flights))
		)
	)

(defn process-customer [{:keys [id from to seats budget]}]
	(let [flight (find-cheapest-flight-with-n-seats-and-within-budget from to seats budget)]

		  (if (nil? flight)
		  	(log "Customer" id "did not find a flight")
		  	(let [price (cheapest-available-price flight budget seats)]
		  		(if (buy-seats flight seats price)
		  			(log "Customer" id "booked" seats "seat(s) for $" price "with budget $" budget "on flight" (:id @flight))
		  			(do (reset! recurcount (+ @recurcount 1)) (recur {:id id :from from :to to :seats seats :budget budget})))
		  		)
		  	)
		))

(defn sales-process []
	"The sales process starts and ends sales periods, until `done?`
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

(defn process-customers [customers]
	(doall (pmap process-customer customers))
	(reset! done? true))

(defn -main []
	(init-flights input/flights)
	(init-flights-per-carrier input/carriers)
	(let [sales-loop (future (sales-process))
		  customers-loop (future (time (process-customers input/customers)))]

	; (println (buy-seats (first @flights) 4 (first (first (:pricing @(first @flights))))))
	; (println (first (find-flights-with-n-seats-and-within-budget "BRU" "MAD" 5 1000)))
	; (println (cheapest-available-price 
	; 	(first (find-flights-with-n-seats-and-within-budget "BRU" "MAD" 5 1000))
	; 	5 1000))
	; (println (find-cheapest-flight-with-n-seats-and-within-budget "BRU" "MAD" 5 1000))

	;(process-customer (first input/customers))
	@customers-loop
	@sales-loop
	;(println "----")
	; (print-flights @flights)
	;(println "----")
	(println @recurcount)
	(shutdown-agents)
	)
)
