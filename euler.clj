;; Many of these are revised solutions copied or borrowed from
;; http://clojure-euler.wikispaces.com

;; Problems are found at projecteuler.net
;;
;; Tools used are VimClojure plus the cljr repl with liberal use of (load-file "euler.clj")

(defn reload
  "Reload this file"
  []
  (load-file "/home/daniel/myapps/euler.clj")
  )

(defn problem-1
      "Sum all multiples of 3 or 5 below 1000"
      []
      (reduce +
	      (distinct
		(lazy-cat
		  (range 3 1000 3)
		  (range 5 1000 5))))) 

(defn fibos
      "lazy seq of fibonacci numbers - alg from cgrand"
      []
      (map first
	   (iterate
	     (fn [[a b]] [b (+ a b)] 
		 )
	     [0 1]))) 

(defn problem-2
      "Sum all fibonacci numbers under four million."
      []
      (reduce +
	      (filter even?
		(take-while
		#(< % 4e6)
		(fibos)))))

(defn -divisible?
      "Determine whether or not f is a factor of n"
      [n f]
      (zero? (rem n f)))

(defn prime?
      "Determine whether n is a prime number"
      [n]
      (cond
	(or (= 2 n) (= 3 n))	        true
	(or (-divisible? n 2) (< n 2))	false
	:else                           (let [sqrt-n (Math/sqrt n)]
                                   (loop [i 3]
                                     (cond
                                       (-divisible? n i)  false
                                       (< sqrt-n i)       true)))))

(defn problem-3
  "Calculate the largest prime factor of [n]"
  [n]
  (first
    (filter #(and (zero? (rem n %))
                  (prime? %))
            (range (int (Math/sqrt n)) 0 -1))))

(defn problem-4
  []
  (apply max (filter #(= (str %)
                         (apply str (reverse (str %))))
                                    (for [x (range 100 1000)
                                          y (range 100 (+ x 1))]
                                      (* x y)))))

(defn first-uneven-divisor
  [n]
  (first (filter pos?
               (seq (for [x (iterate #(inc %) 1)]
                      (cond (pos? (rem n x)) x
                            :else              0))))))

(defn problem-5
  []
(first 
  (filter #(> (first-uneven-divisor %) 20)
          (iterate #(+ 10 %) 10))))

(defn sum-of-squares [n] (reduce + (map #(* % %) (range 1 (+ 1 n)))))

(defn square-of-sum [n] (int (Math/pow (reduce + (range 1 (+ 1 n))) 2)))

(defn problem-6 [] (- (square-of-sum 100) (sum-of-squares 100)))

(defn nthprime [n] 
    (nth 
      (filter prime? 
              (iterate #(inc %) 1)) (- n 1)))


(defn problem-7 [] (nthprime 10001))

(def *bignum-8* "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(def *input-8* (map #(Integer. (str %)) (seq *bignum-8*))) 

(defn problem-8
  []
  (apply max (map (partial reduce *)
                  (partition 5 1 (map #(Integer. (str %))
                                      (seq *bignum-8*))))))
(defn hypotenuse-float [a b]
  (Math/sqrt (+ (Math/pow a 2) (Math/pow b 2))))

(defn hypotenuse [a b]
  (let [h (hypotenuse-float a b)]
    (cond (= h (int h)) (int h)
          :else         nil)))

(defn input-9
  []
  (for [x (range 1 1001) y (range x 1001)]
    [(hypotenuse x y) x y]))

(def p9-1000s (filter #(number? (first %)) (input-9)))

(defn problem-9-solver
  []
  (reduce * (first (filter #(= 1000 (reduce + %)) p9-1000s))))

