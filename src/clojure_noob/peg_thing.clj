(ns clojure-noob.peg-thing
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over prompt-rows)

;create a lazy sequence of triangular numbers which is a
;series summing the natural numbers ex: 1, 3 (1+2), 6 (1+2+3),...
(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
    (let [new-sum (+ sum n)]
      (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

;binding this lazy sequence to tri
(def tri (tri*))
;test it by calling (take 5 tri)

;next to determine if a number is a triangular number, which
;denotes that it is at the edge of the board.
(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15,..."
  [n]
  (= n (last (take-while #(>= n %) tri))))

;row-tri takes a row number and gives the triangular number at
;it's end
(defn row-tri
  "The triangular number the the end of row n"
  [n]
  (last (take n tri)))

;and lastly, row-num which takes in a board position and returns
;the row number containing that position
(defn row-num
  "Returns row number the postition belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

;connect forms a connection between two positions on the board
(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))
;if the connection request is outside the bounds of the board,
;this function returns the board with no changes.
;test with (connect {} 15 1 2 4)

;now we need to find a way to let the program which connections
;to make between any two positions

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

;these functions take a board, it's max size and a position and
;determines which numbers to feed into connect using some "triangle
;math".  The reason we only need to go one way with these functions
;is that connect makes mutual connections so when 4 is connected to 6,
;6 is also connected to 4.
;example: (connect-down-left {} 15 1) (connect-down-right {} 15 3)

;the next function add-pos actually reduces on a vector of functions
;applying each in turn to build up on the resulting board.  It first
;updates the board to indicate that a peg is in a given position.

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))
;test with (add-pos {} 15 1)
;this function first adds a peg to the board at position X, the reduce
;fucntion then takes this new board configuration and attempts to connect
;position X to a legal rightward position, then takes that resulting board
;and attempts to connect X to legal down-left positions, and finally once
;again for down-right connections.  This final resultant board is returned

;Note: there are few places where you will need to reduce over a collection
;of functions but it can be very useful when the need arises.

;Finally we can no create our board
(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

;this creates the initial, empty board and gets the max-position.
;it then gets a list of numbers 1-max-pos otherwise known as the
;board's positions.

;;;;;;
;MOVING PEGS
;;;;;;

;this next section validates and performs peg moves, many of
;these functions are self explanitory oneliners

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the baord at given positioin"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it into p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

;Notice how nice this code is.  There is no mutation of data as in OOP,
;only transformation of data via mapping functions onto data.
;The best part is, there really is no need to make a class to contain
;these guys.  This is, IMO, my favorite part of functional programming.

;next we need to validate our moves on the board:

(defn valid-moves
  "Return a map of all valid moves for a given pos, where the key is
  the destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))
;to test first make a board
;(def my-board (assoc-in (new-board 5) [4 :pegged] false))
(def my-board (assoc-in (new-board 5) [4 :pegged] false))
;then validate moves within (valid-moves my-board 1)

;valid-moves returns a map rather than a vector, this is to
;allow for an easy lookup to a destination position to check whether a
;specific move is valid, which is what valid-move? does:
(defn valid-move?
  "Return a jumped position if the move from p1 to p2 is valid,
  nil otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))
;test with (valid-move? my-board 8 4) => nil
;(valid-move? my-board 1 4) => 2

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

;uses if-let which will perform a let on the truthy case but will
;just perfom the else option if the conditional fails. For example:
;if there is a valid move then the output of (valid-move? board p1 p2)
;is mapped to jumped


(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))
;question mark notations indicate a predicate function, iow a function
;that is meant to be used in Boolean expressions.
;can-move? gathers a sequence of all pegged positions on the board.
;filter converts the board (a map) into a seq of two element vectors.
;Which look like this:
;([1 {:connections {6 3, 4 2}, :pegged true}]
; [2 {:connections {9 5, 7 4}, :pegged true}])
;filter then applies the anonymous function #(get (second &) :pegged)
;to each element, filtering out the vectors where the position's info
;indicates that the position is not currently housing a peg.  Finally,
;the result is passed to map which calls first one each vector to grab
;just the position number from each vector in the seq.

;after this seq of pegged positions is found, a predicate function is
;called on each one to find the first position that returns a truthy
;value.  The predicate function is created from:
; ( comp not-empty (partial valid-moves board))
;the objective here is to return a map of all valid moves for a pos
;and determine if that map is empty.

;the expression (partial valid-moves board) derives an anonymous function
;from valid-moves with the first argument, board, filled in using partial
;(because we are using the same board each time we call valid-moves)
;The new function can take a position and return the map of all its valid
;moves for the current board.

;then we use comp to compose this function with not-empty.  This function
;is self-descriptive; it returns true if the given collection is empty
;and false otherwise.

;the neatest bit about this code is it is using a chain of functions
;to derive a new function, similar to how chains of functions are used
;to derive new data (we're geting real meta here).

;also, see tuples if you got lost on the first paragraph

;on to rendering the board!

;;;;;
;Rendering
;;;;;

;these first few expressions are defining some constants needed
;see, ASCII
(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))

;working from the bottom of this block upwards, you can see that
;render-row is calling each of the functions above to return the
;string representation of the given row.

;finally, print-board (below) iterates over each row number with doseq
;printing the string representation of that row.

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

;;;;
;Player Interaction
;;;;

(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
    (let [input (clojure.string/trim (read-line))]
      (if (empty? input)
        default
        (clojure.string/lower-case input)))))

;The get-input functon is a helper function which allows you to read
;and clean a player's input.  This also allows for you to use a default
;input to enter when the user doesn't input anything.

(defn characters-as-strings
  "Given a string, retrun a collection consisting of each individual
  character"
  [string]
  (re-seq #"[a-zA-Z]" string))

;this helper function is used by prompt-move to take in strings and
;return a collection of letters with all non alphabetic input discarded

(defn prompt-move
  "Reads the player's input and acts on it"
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (successful-move new-board)
      (do
        (println "\n!!! That was an invalid move :(\n")
        (prompt-move board)))))

(defn successful-move
  "Checks to see if any valid moves can be made and if not, game over"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

;the make-move in the if-let will attempt to make a move and will return
;nil if the player made an invalid move.  The board is then passed to
;one of two functions to inform the player if they either made a mistake
;or prompt for their next move and pass current or updated board
;respectively.

;if-let sounds a lot like a try catch in C# based on this example

(defn user-entered-invalid-move
  "DEPRECATED: Handles the next step after a user has entered an invalid move"
  [board]
  (println "\n!!! That was an invaid move :(\n")
  (prompt-move board))

;however if a valid move is given the board is passed back into
;prompt-move if there are still moves to be played otherwise, game over

(defn user-entered-valid-move
  "DEPRECATED: Handles the next step after a user has entered a vaid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

;In the board creation functions above, recursion was uses to build up a
;value using imutable data structures.  The same is happening here, only
;it involves two mutually recursive functions and some user input.

(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game Over!  You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

;The game informs the player how they did, prints the final board and
;prompts them to play again.  If yes, these last two functions are
;called which are used to start a new game.

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn -main
  [& args]
  (println "Get ready to play peg thing!")
  (prompt-rows))