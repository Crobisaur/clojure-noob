(ns clojure-noob.cis100)
;clojure adaptation of the TIS-100 Tesselated Intelligence System

;don't define states like you do here, instead add recursive
;calls that decrease the state duration
;(def process-step {:single 1 :double 2})

(def limits {:upper 999 :lower -999})

;check if value is within limits of machine
(defn check-value [value]
  (if (> value (:upper limits))
    (:upper limits)
    (if (< value (:lower limits))
      (:lower limits)
      value)))

;adds two registers together, values of a register cannot exceed
;+999 or -999 likewise their sum cannot exceed this range
(defn ADD [A-register B-register]
  (check-value (+ (check-value A-register)
                  (check-value B-register))))
