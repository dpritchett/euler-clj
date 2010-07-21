;; Many of these are revised solutions copied or borrowed from
;; http://clojure-euler.wikispaces.com

;; Problems are found at projecteuler.net
;;
;; Tools used are VimClojure plus the cljr repl with liberal use of (load-file "euler.clj")

(defn reload
  "Reload this file"
  []
  (load-file "euler.clj")
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

(defn divisible?
      "Determine whether or not f is a factor of n"
      [n f]
      (zero? (rem n f)))

(defn prime?
      "Determine whether n is a prime number"
      [n]
      (cond
	(or (= 2 n) (= 3 n))	        true
	(or (divisible? n 2) (< n 2))	false
	:else                           (let [sqrt-n (Math/sqrt n)]
                                   (loop [i 3]
                                     (cond
                                       (divisible? n i)  false
                                       (< sqrt-n i)      true
                                       :else             (recur (+ i 2)))))))

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

(defn first-unevendivisor
  [n]
  (first (filter pos?
               (seq (for [x (iterate #(inc %) 1)]
                      (cond (pos? (rem n x)) x
                            :else              0))))))

(defn problem-5
  []
(first 
  (filter #(> (first-unevendivisor %) 20)
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

(defn primes
  "Lazy sequence of prime numbers.  v0 is very naive"
  []
  (filter prime? (iterate #(inc %) 2)))

(defn problem-10
  "The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million."
  []
  (reduce + (take-while #(> 2E6 %) (primes))))

(def *input-11*
  "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
  81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
  52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
  22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
  24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
  32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
  67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
  24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
  21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
  78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
  16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
  86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
  19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
  04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
  88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
  04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
  20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
  20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
  01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(def *input-11* (partition 20 (map #(Integer. %) (seq (.split *input-11* "\\s+")))))

;;accessor
(defn by-coords [x y] (nth (nth *input-11* y '()) x 0))

(defn diag-rfourple [x y] (reverse (conj '() (by-coords x y) (by-coords (+ x 1) (+ y 1)) (by-coords (+ x 2) (+ y 2)) (by-coords (+ x 3) (+ y 3)))))

(defn diag-lfourple [x y] (reverse (conj '() (by-coords x y) (by-coords (- x 1) (+ y 1)) (by-coords (- x 2) (+ y 2)) (by-coords (- x 3) (+ y 3)))))

;; Max product of four consecutive numbers from the first row
(apply max (map #(reduce * %) (partition 4 1 (first *input-11*))))

;; Vertical tuples of 4
(def *input-11-vert-tuples*
  (map #(partition 4 1 %)
       (for [x (range 0 20)] (map #(nth % x) *input-11*))))

;; Diagonal tuples of 4
(def rdiag-max-11
  (apply max 
         (for [x (range 0 20) y (range 0 20)]
           (reduce * (diag-rfourple x y)))))

;; Diagonal tuples of 4
(def ldiag-max-11
  (apply max 
         (for [x (range 0 20) y (range 0 20)]
           (reduce * (diag-lfourple x y)))))

;; max product of vertical tuples of 4
(def vert-max-11
  (apply max
       (map #(apply max %)
            ((fn [i]
               (map (fn [j] (map #(reduce * %) j)) i))
               *input-11-vert-tuples*))))

;; max product of horizontal tuples of 4
(def horiz-max-11
  (apply max
       (map #(apply max %)
            ((fn [i]
               (map (fn [j] (map #(reduce * %) j)) i))
               (map #(partition 4 1 %) *input-11*)))))

(defn problem-11
  []
  (max horiz-max-11 ldiag-max-11 vert-max-11 rdiag-max-11)
  )

(defn triangles
  "lazy seq of triangle nums, based on cgrand's fibos alg used above"
  []
  (map first
       (iterate
         (fn [[t n]] [(+ t n) (inc n)]) [1 2])))

(defn triangles
  "chouser's improved version of above"
  []
  (reductions + (iterate inc 1)))
