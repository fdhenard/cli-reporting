(ns cli-reporting.core
  (:require [clojure.math.numeric-tower :as math]))

(defn- current-time
  "Return the current time. Same as (.getTime (java.util.Date.))."
  []
  (.getTime (java.util.Date.)))


(defn- hrs-min-sec [milli-secs]
  (let [secs (/ milli-secs 1000)
        secs-rem (float (rem secs 60))
        mins (/ secs 60)
        mins-rem (math/round (rem mins 60))
        hrs (math/round (/ mins 60))
        hrs-str (if (> hrs 0)
                  (str hrs " hrs ")
                  "")
        mins-str (if (> mins-rem 0)
                   (str mins-rem " mins ")
                   "")
        secs-str (if (> secs-rem 0)
                   (str secs-rem " seconds")
                   "")
        ]
    (str hrs-str mins-str secs-str)))



(defn- measure-coll-retrieval
  "Returns a lazy seq of items from coll. Will print to stdout the
  average time between element extractions."
  ;; you can use destructuring inside the argument list as well,
  ;; here
  ;; we're additionaly specifying some defaults for when the caller
  ;; does not provide a value
  [coll rpt-every & {:keys [start-count total-count] :or {start-count 0 total-count nil}}]
  (let [beginning-time (current-time)]
    (map-indexed
     (fn [index item]
       (let [index (+ start-count index 1)
             average-time (/ (- (current-time) beginning-time) index)]
         (if (= 0 (mod index rpt-every))
           (do
             (print "Avg time / each = " (hrs-min-sec average-time))
             (if total-count
               (println ", Estimated total time left = "
                        (hrs-min-sec (* average-time (- total-count index))))
               (println))))
         item))
     coll)))

(defn- unchunk
  "takes a chunked sequence and turns it into an unchunked sequence"
  [s]
  (lazy-seq
   (when-let [[x] (seq s)]
     (cons x (unchunk (rest s))))))


(defn- take-until
  "Returns a lazy seq of items from coll util the STOP-TIME has been reached."
  [stop-time coll]
  (take-while
   (fn [item]
     (if (not (> (current-time) stop-time))
       true
       (do (println "stopped at " (current-time) ". Requested stop at " stop-time)
           false)))
   coll))

(def one-day (* 1000 60 60 24))

(defn run-with-reporting [sequence-function-maps & {:keys [stop-time rpt-every]
                                                    :or {stop-time nil rpt-every 1}}]
  (let [beginning-time (current-time) ;; 1.
        seq-counts (map (comp count :sequence) sequence-function-maps)
        total-sequence-counts (reduce + seq-counts)
        intermed-count (atom 0)
        stop-time (if (nil? stop-time)
                    (+ (current-time) one-day)
                    stop-time)]
    (println (str "total counts = " total-sequence-counts ". reporting every " rpt-every))
    ;; note how it is possible to immediately destructure the map
    (doseq [{:keys [sequence function]} sequence-function-maps]
      (swap! intermed-count
             +
             (count
              ;; this is where everything happens: apply
              ;; function,
              ;; take-until stop-time reached & measure
              ;; average time
              (measure-coll-retrieval
               (take-until stop-time (map function (unchunk sequence)))
               rpt-every
               :start-count @intermed-count
               :total-count total-sequence-counts))))
    (println "Total time taken = " (hrs-min-sec (- (current-time) beginning-time))
             ", Done = " @intermed-count
             "/" total-sequence-counts)))

(defn test-run-w-reporting [& {:keys [stop-time-in-mins]
                               :or {stop-time-in-mins nil}}]
  (let [testing-func (fn [item] (. java.lang.Thread sleep (rand 1000)))
        sequence-function-maps [{:sequence (range 30), :function testing-func},
                                {:sequence (range 15), :function testing-func}]
        stop-time-in-millis (if stop-time-in-mins
                              (+ (current-time) (* 1000 60 stop-time-in-mins))
                              nil) ]
    (run-with-reporting sequence-function-maps :stop-time stop-time-in-millis :rpt-every 4)))
