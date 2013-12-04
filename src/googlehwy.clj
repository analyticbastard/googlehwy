(ns googlehwy)

(defn digits-of-e
  "Compute the digits of the Euler number by "
  [N n]
  (if (= n N)  (.setScale  2.5M 200)
    (if (= n 0) 
      (+ 2.0M (* (.divide 
                   (.setScale 1.0M 200 BigDecimal/ROUND_HALF_UP)
                   2.0M BigDecimal/ROUND_HALF_UP) 
                 (digitse N (inc n) )) )
      (+ 1.0M (* (.divide 
                   (.setScale 1.0M 200 BigDecimal/ROUND_HALF_UP) (+ n 2.0M)
                   BigDecimal/ROUND_HALF_UP) 
                 (digitse N (inc n)))) ) ))

; Compute e to a sufficiently large number of iterations to get 100 correct digits
(def e (digits-of-e 150 0))

; Prune the scale of the BigDecimal number and convert to string
(def se (.subSequence (.toString e) 0 150 ))


(defn gen-primes
  "Generates an infinite, lazy sequence of prime numbers"
  []
  (let [reinsert (fn [table x prime]
                   (update-in table [(+ prime x)] conj prime))]
    (defn primes-step [table d]
      (if-let [factors (get table d)]
        (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
               (inc d))
        (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                       (inc d))))))
    (primes-step {} 2)))



; Generate sufficient primes to cover the square root of a 10-digit number (or about it)
(def first-primes (take 8500 (gen-primes) ) )


(defn prime-restricted?
  "Search if a given number is in a list of primes"
  [n]
  (= nil (some #(= 0 (rem n %)) first-primes) ))  


(defn find-first-prime-in-e [se init]
  "Iterate over the string of numbers and check whether a 10-digit number is prime"
  (if (<= (- (.length se) init) 10)
    nil 
    (let [number (java.lang.Long/parseLong (.subSequence se init (+ init 10)) )]
      (println init number)
      (if (prime-restricted? number first-primes)
        number
        (recur se (inc init))
        )))
  )

