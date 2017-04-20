(ns clojure-noob.cis-read)
;A parser to read in a file containing assembly instructions
;in the style of TIS-100 specifications

(def filename "resources/cis_terminal.cis")

(slurp filename)

(defn str->int
  [str]
  (Integer. str))

(defn parse
  "Convert a CIS file into instructions for the CIS-100"
  [string]
  (map #(clojure.string/split % #" ")
       (clojure.string/split string #"\n")))
;TODO: implement a way to parse out MOV x, y instructions
;no need to look for the comma as MOV only moves whatever is in
;ACC to the specified port

(defn lexify
  "Lexer to construct the functions based on instructions
   given in CIS file"
  [instructions]
  )

(defn lex [instruction]
  (if ))