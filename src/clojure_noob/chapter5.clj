(ns clojure-noob.chapter5)

;declaring a referentially transparent function
(defn wisdom
  [words]
  (str words, ", Daniel-san"))
;meaning any given inputs will always return the same output

;as a contrasting example the following is not referentially
;transparent since it's using RNG.
(defn year-end-evaluation
  []
  (if (> (rand) 0.5)
    "You get a raise!"
    "Too bad, chump."))
;note that the same input will not gurantee the same output
;every time.  Any function that uses RNG is by default not
;referentially transparent.

;likewise this can apply to inputs, for example reading a file
;is not referentially transparent as the contents of said file
;can change.  However, the analysis function of contents extracted
;from a given file is referentially transparent.

(defn analysis
  [text]
  (str "Charactyer count: " (count text)))
;^referentially transparent

(defn analyze-file
  [filename]
  (analysis (slurp filename)))
;^not referentially tranparent

;defining a recursive sum function
(defn sum
  ([vals] (sum vals 0))
  ([vals accumulating-total]
    (if (empty? vals)
      accumulating-total
      (sum (rest vals) (+ (first vals) accumulating-total)))))

;an example of how this would unfold recursively
;(sum [39 5 1]) ;single-arity body calls two-arity body
;(sum [39 5 1] 0)
;(sum [5 1] 39)
;(sum [1] 44)
;(sum [] 45) ;base case is reached, so return accumulating-total
; => 45

;for reasons best not expanded upon recursive functions should
;use 'recur' for better performance.  Clojure does not provide
;tail call optimization (not worth reading about).

(defn sum-mkII
  ([vals]
    (sum-mkII vals 0))
  ([vals accumulating-total]
    (if (empty? vals)
      accumulating-total
      (recur (rest vals) (+ (first vals) accumulating-total)))))

;granted this optimization is not neccesary if you intend to
;use this function on small collections.  It is necessary if
;collection sizes exceed thousands or millions of values in order
;to avoid stack overflow errors.

;and intermediate values in recursion loops do not have a
;significant impact on performance due to clojures immutable
;data structures.
;see: http://hypirion.com/musings/understanding-persistent-vector-pt-1

;;Function composition instead of attribute mutation
; in the tutorial a javascript implementation of a glamour shot
;caption cleaner.  It scans text for any instance of 'lol' and
;replaces it with the all-caps version 'LOL'
;it also removes trailing whitespace from the string

;here's the functional approach to this operation
(require '[clojure.string :as s])
(defn clean
  [text]
  (s/replace (s/trim text) #"lol" "LOL"))

;example input: (clean "My spoon is Too bIG, lol     ")
; => "My spoon is Too bIG, LOL"

;the input string is passed in as an immutable object to s/trim
;which outputs an immutable string with whitespace trimmed from
;the end.  This value is then passed into the s/replace function
;which, you guessed it, returns an immutable string.

;as an aside, there is a strong similarity

;learning about partial, comp and memoize
;comp let's you compose a new function from a set of other
;functions, for example
;((comp inc *) 2 3)
; => 7
;would return 7 as the numbers two and three would be multiplied
;together and the result is incremented by one.  Note that *
;may take any number of arguments while inc will only take one
;Comp may also be used in a multitude of ways, this example shows
;how it may be used to retrieve character stats from an RPG.

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})
(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))

;calling any of the c-* functions will return a character's
;corresponding attributes

