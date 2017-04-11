(ns clj-util.core
  "Utility functions for Clojure"
  (:require
   [clojure.core.async :refer [>! alts!! timeout chan go]]
   [clojure.reflect]
   [clj-time.core :as t]
   ))

(defn aseq
  "Ensure result is a sequence. Suggested by 0dB on SOF to avoid
  annoying Incanter Dataset sel edge case."
  [a] (if (seq? a) a (list a)))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn uuid-int [] (rand-int (Integer/MAX_VALUE)))

(defmacro dbg "Debugging parts of expressions."
  [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defmacro dbgsym
  "Debugging a symbol by interning it in the 'dbg namespace, creating
  that namespace if it doesn't exist already."
  [x] `(let [x# ~x]
         ;; (println (str "dbg/" '~x) "=" x#)
         (println (str "DEBUGGING dbg/" '~x))
         (create-ns (symbol "dbg"))
         (intern (symbol "dbg") '~x x#) x#))

(defmacro dbgtim "Print time taken by expressions."
  [x] `(let [start# (t/now)
             x# (do ~x)
             ret# (str x#)]
         (println (str "dbg (" (t/in-seconds (t/interval start# (t/now))) "s):")
                  '~x (str "(len=" (count ret#) ")")) x#))

(defmacro dbgtimval "Debugging parts of expressions, including the time they take."
  [x] `(let [start# (t/now)
             x# (do ~x)
             ret# (str x#)]
         (println (str "dbg (" (t/in-seconds (t/interval start# (t/now))) "s):")  '~x "=" ret#) x#))

(defmacro defns
  "Define a function in a given namespace."
  [namespace name & body]
  (if (instance? String (first body))   ; Was a docstring given?
    ;; TODO Retain a given docstring; don't discard it.
    `(intern ~namespace ~name
             (fn ~(rest body)))         ; Discard docstring
    `(intern ~namespace ~name
             (fn ~body))))

(defn only
  "Get the only entry of a list, else raise an error."
  [x]
  {:pre [(nil? (next x))]} (first x))

(defn printlnw
  "Like println, but wrap any lines over a maximum width"
  [string width]                      
  (defn string-wrap [s w]
    (if (= 0 (count s))
      nil
      (lazy-seq (cons (apply str (take w s))
                      (string-wrap (drop w s) w)))))
  (doseq [origline (clojure.string/split (str string) #"\n")]
    (doseq [line (string-wrap origline width)]
      (println line))))

(defn print-members-and-return-types
  "Print a table showing the members and return types of an object."
  [object]
  (println (type object))
  (->> (clojure.reflect/reflect object)
       :members
       (sort-by :name)
       (clojure.pprint/print-table [:name :return-type])))

(defn print-reflection
  "Print reflection information about an object, e.g., its methods."
  [object]
  (clojure.pprint/pprint (clojure.reflect/reflect object)))

(defn string-center 
  "Surround a string with padding, producing a string a of length width."
  [string padding width]
  (let [pad-wid (-> width (- (count string)) (- 2) (/ 2))]
    (apply str
           (apply concat
                  [(repeat pad-wid padding)
                   " "
                   string
                   " "
                   (repeat pad-wid padding)]))))

(defn println-center [string padding width]
  "Print a string after surrounding it with padding so it a certain width."
  (println (string-center string padding width)))

(defn random-sample-n
  "Return a random sample of the given size from the given
  collection without replacement"
  [coll n]
  {:pre [(>= (count coll) n)]}
  (cond (= (count coll) n) coll
        :else 
        (take n
              (shuffle coll))))

(defn random-sample-perc
  "Return a random sample of the given percent from the given
  collection without replacement"
  [coll perc]
  (let [n (-> perc
              (/ 100.0)
              (* (count coll)))]
    (cond (= 0 n) []
          :else 
          (take n
                (shuffle coll)))))

(defn with-timeout
  "From http://stackoverflow.com/questions/6694530/
  executing-a-function-with-a-timeout"
  [ms f & args]
  (let [c (chan)]
    (go (>! c (apply f args)))
    (first (alts!! [c (timeout ms)]))))

(defn with-timeout-exception
  "From http://stackoverflow.com/questions/6694530/
  executing-a-function-with-a-timeout.  Also, throw an exception if
  the timeout occurs, rather than just returning nil."
  [ms f & args]
  (let [c (chan)
        fw (fn [f & args] {:result (apply f args)})]
    (go (>! c (apply fw f args)))
    (let [result (first (alts!! [c (timeout ms)]))]
      (when (nil? result)
        (throw (Exception. (str "Timeout after " ms " ms"))))
      (:result result))))
