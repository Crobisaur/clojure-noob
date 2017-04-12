(ns clojure-noob.cis100)
;clojure adaptation of the TIS-100 Tesselated Intelligence System

;don't define states like you do here, instead add recursive
;calls that decrease the state duration
;(def process-step {:single 1 :double 2})

(def limits {:upper 999 :lower -999})
(def register-keys [:acc
                    :sav
                    :top
                    :bottom
                    :left
                    :right])

(def registers {:acc    0
                :sav    0
                :top    0
                :bottom 0
                :left   0
                :right  0})


;check if value is within limits of machine
(defn check-value [value]
  (if (> value (:upper limits))
    (:upper limits)
    (if (< value (:lower limits))
      (:lower limits)
      value)))



;adds two registers together, values of a register cannot exceed
;+999 or -999 likewise their sum cannot exceed this range
(defn ADD [value]
  (update-acc [(check-value
                 (+ (check-value value)
                    (registers :acc)))]))

(defn SUB [value accumulator]
  (check-value (- (check-value value)
                  (check-value accumulator))))

(defn get-registers
  "Returns the correspoding register (key) values."
  [& keys]
  (into [] (map registers keys)))

(defn update-acc [new-val]
  (update-in registers [:acc] new-val))

(defn update-registers [new-vals]
    (reduce (fn [new-registers [register-keys values]]
              (map register-keys new-vals))
            {}
            registers))
(defn get-acc
  "Retrieves the value stored in the accumulator (ACC)"
  [registers]
  (let [[accumulator _] registers]
    accumulator))

(defn SWP1
  "Takes in the accumulator (ACC) and save (SAV) registers
  and swaps their values."
  [accumulator save]
  (reverse [accumulator save]))

(defn SWP2
  [registers]
  (let [[_ save] registers]
    save))

