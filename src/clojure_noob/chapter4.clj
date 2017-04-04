(ns clojure-noob.chapter4)

(defn titleize
  [topic]
  (str topic " for the Brave and True"))

(def human-consumption [8.1 7.3 6.6 5.0])
(def critter-consumption [0.0 0.2 0.3 1.1])
(defn unify-diet-data
  [human critter]
  {:human human
   :critter critter})

(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))
(defn stats
  [numbers]
  (map #(% numbers) [sum count avg]))

(def identities
  [{:alias "Batman" :real "Bruce Wayne" }
   {:alias "Spider-Man" :real "Peter Parker"}
   {:alias "Santa" :real "Your mom"}
   {:alias "Easter Bunny" :real "Your dad"}])

;using a map to transform an existing map's values
(reduce (fn [new-map [key val]]
          (assoc new-map key (inc val)))
        {}
        {:max 30 :min 10})

(reduce (fn [new-map [key val]]
          (if (> val 4)
            (assoc new-map key val)
            new-map))
        {}
        {:human 4.1
         :critter 3.9})

;this data set is used to test the take-while and drop-while functions
;ex: (take-while #(< (:month %) 3) food-journal)
;ex: (drop-while #(< (:month %) 3) food-journal)
;take-while pulls out entries until a false test is encountered
;drop-while drops entries until a true test is encountered
;filter returns all entries that satisfy the condition
;this may or may not be as efficient than take or drop since you will
;always iterate through the entire collection with a filter

(def food-journal
  [{:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 2.0}
   {:month 2 :day 1 :human 4.9 :critter 2.1}
   {:month 2 :day 2 :human 5.0 :critter 2.5}
   {:month 3 :day 1 :human 4.2 :critter 3.3}
   {:month 3 :day 2 :human 4.0 :critter 3.8}
   {:month 4 :day 1 :human 3.7 :critter 3.9}
   {:month 4 :day 2 :human 3.7 :critter 3.6}])

(def vampire-database
  {0 {:makes-blood-puns? false, :has-pulse? true :name "McFiswich"}
   1 {:makes-blood-puns? false, :has-pulse? true :name "McMackson"}
   2 {:makes-blood-puns? true, :has-pulse? false :name "Damon Salvatore"}
   3 {:makes-blood-puns? true, :has-pulse? true :name "Mickey Mouse"}})

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))

(defn vampire?
  [record]
  (and (:makes-blood-puns? record)
       (not (:has-pulse? record))
       record))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire?
                 (map vampire-related-details social-security-numbers))))

;batman generator
;(concat (take 8 (repeat "na")) ["Batman!")
; defining a lazy (unrealized) infinite sequence of even unmbers
; unrealized means that the contents of this defined data aren't actually
; stored/created until they are needed
(defn even-numbers
  ([] (even-numbers 0))
  ([n] (cons n (lazy-seq (even-numbers (+ n 2))))))
; this is a bit overwhelming due to it's recursive nature
; just remember that cons returns a new list with an element appended
; to the given list
; (cons 0 '(2 4 6)) => (0 2 4 6)


;moving on to collections
;(map identity {:sunlight-reaction "Glitter!"})

;major differences between into and conj is that conj passes
;the second argument as a non-collection value whereas the
;second argument of an into must be a collection

;a good way to show how similar they are is to use into to
;define conj

(defn my-conj
  [target & additions]
  (into target additions))

;onto functions
; apply and partial
;apply simply applies the given function onto the following structure
;or function
;ex: (apply max [0 1 2]))

;using apply, we can now define into in terms of conj
(defn my-into
  [target additions]
  (apply conj target additions))

;partial will take a funciton and any number of arguments
;and returns a new function

(def add10 (partial + 10))

(def add-missing-elements
  (partial conj ["water" "earth" "air"]))

;likewise let's define our own partial
(defn my-partial
  [partialized-fn & args]
  (fn [& more-args]
    (apply partialized-fn (into args more-args))))

;and our own defined my-partial function
(def add20 (my-partial + 20))

;this weirdness is useful in cases where you are often repeating
; the same combination of function and arguments in many different
;contexts. For example:

(defn lousy-logger
  [log-level message]
  (condp = log-level
    :warn (clojure.string/lower-case message)
    :emergency (clojure.string/upper-case message)))

(def warn (partial lousy-logger :warn))

;complement is the mathematical term for "the negation of/...
;other than/opposite" this example will find all the humans
;in our database created above

(defn identify-humans
  [social-security-numbers]
  (filter #(not (vampire? %))
          (map vampire-related-details social-security-numbers)))

(def not-vampire? (complement vampire?))
(defn identify-humans
  [social-security-numbers])