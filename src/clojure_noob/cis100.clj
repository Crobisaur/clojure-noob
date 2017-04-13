(ns clojure-noob.cis100)
;clojure adaptation of the TIS-100 Tesselated Intelligence System
;this was created in an attempt to better understand clojure,
;functional programming, and some assembly language practices.

;don't define states like you do here, instead add recursive
;calls that decrease the state duration
;(def process-step {:single 1 :double 2})

(def limits {:upper 999 :lower -999})
(def register-keys [:acc
                    :bak
                    :top
                    :bottom
                    :left
                    :right])

;define the initial condition for registers in any process block
;TODO; Add a :cycle key to record how many operations have occured
(def registers {:ACC 0
                :BAK 0
                :TOP nil
                :BTM nil
                :LFT nil
                :RGT nil
                :NIL 0
                :CYC 0})

;check if value is within limits of machine
(defn check-value [value]
  (if (> value (:upper limits))
    (:upper limits)
    (if (< value (:lower limits))
      (:lower limits)
      value)))

;adds two registers together, values of a register cannot exceed
;+999 or -999 likewise their sum cannot exceed this range

(defn NOP
  "Performs a no operation.  Does nothing, costs 1 cycle"
  [registers]
  (update-in registers [:CYC] inc))

(defn SWP
  "Takes in all registers and swaps the values stored in
   the accumulator and save registers, it is not permitted
   to update :BAK directly"
  [registers]
  (assoc registers :ACC (registers :BAK)
                   :BAK (registers :ACC)
                   :CYC (+ (registers :CYC) 1)))
;(clojure.set/rename-keys registers
;(update-in registers [:sav :acc] (select-keys registers [:acc :sav]))

(defn SAV
  "Copies accumulator value into backup register, writing
   directly to backup register is not allowed"
  [registers]
  (assoc registers :BAK (registers :ACC)
                   :CYC (+ (registers :CYC) 1)))

(defn ADD
  "Takes in any integer and adds it to the accumulator
  register :ACC"
  [value registers]
  (update-acc [(check-value
                 (+ (check-value value)
                    (registers :ACC)))]
              registers))

(defn SUB
  "Takes in any integer and subtracts it from the
  accumulator register :ACC"
  [value registers]
  (update-acc [(check-value
                 (- (check-value value)
                    (registers :ACC)))]
               registers))

(defn NEG
  "Negates the value in the accumulator (ACC * -1)"
  [registers]
  (assoc registers :ACC (- (registers :ACC))
                   :CYC (+ (registers :CYC) 1)))

(defn get-registers
  "Returns the correspoding register (key) values."
  [& keys]
  (into [] (map registers keys)))

(defn update-acc-DEP
  "DEPRECATED METHOD"
  [new-val registers]
  (update-in registers [:ACC] new-val))

(defn update-acc
  "Takes in a single element vector and a map of registers
   associates the new-val to :acc and returns a new map of
   updated registers"
  [new-val registers]
  (assoc registers :ACC (first new-val)
                   :CYC (+ (registers :CYC) 1)))

(defn update-top
  "Takes in a single element vector and a map of registers
   associates the new-val to :TOP and returns a new map of
   updated registers"
  [new-val registers]
  (assoc registers :TOP (first new-val)
                   :CYC (+ (registers :CYC) 1)))

(defn update-bottom
  "Takes in a single element vector and a map of registers
   associates the new-val to :BTM and returns a new map of
   updated registers"
  [new-val registers]
  (assoc registers :BTM (first new-val)
                   :CYC (+ (registers :CYC) 1)))

(defn update-left
  "Takes in a single element vector and a map of registers
   associates the new-val to :LFT and returns a new map of
   updated registers"
  [new-val registers]
  (assoc registers :LFT (first new-val)
                   :CYC (+ (registers :CYC) 1)))

(defn update-right
  "Takes in a single element vector and a map of registers
   associates the new-val to :RGT and returns a new map of
   updated registers"
  [new-val registers]
  (assoc registers :RGT (first new-val)
                   :CYC (+ (registers :CYC) 1)))




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

