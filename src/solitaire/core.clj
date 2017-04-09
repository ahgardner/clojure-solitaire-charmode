(ns solitaire.core
  (:require [solitaire.display :as display]
            [solitaire.common :as com]
            [little-game-lib.undo :as undo]
            [little-game-lib.time :as tm]
            [clojure.string :as str]
            [clojure.pprint])
  (:gen-class))

; Klondike solitaire (character mode - no graphics)

; Arthur Gardner Summer 2016

(def cards-in-deck 52)
(def suits-in-deck 4)
(def cards-to-turn 3)
(def initial-cards-in-layout 28)

(def pref-file "solitaire.prefs")

; Initial layout

(defn- deal-layout
  "create columns-in-layout piles,
   where the nth pile contains n cards"
  [deck]
  (vec (for [i (range com/columns-in-layout)
             :let [start (/ (* i (inc i)) 2)]]
         (subvec deck start (+ start i 1)))))

(defn- deal
  "Shuffle and deal, returning a table (game map)"
  []
  (let [deck (shuffle (range cards-in-deck))
        table
        {:pack (subvec deck initial-cards-in-layout)
         :top -1
         :piles (vec (repeat suits-in-deck -1))
         :layout (deal-layout deck)
         :layout-hidden-cards (vec (range com/columns-in-layout))}]
    (tm/start table)))
 
; COMMANDS

; Help command

(defn- help []
  (println)
  (println "N      turn next card from deck")
  (println "P x    play from top of layout pile x (0 for the turned card)")
  (println "P x y  play from layout pile x, starting at row y")
  (println "U x    unplay from ace pile x to layout")
  (println "R      re-deal")
  (println "A      auto-play (from layout, lowest cards first)")
  (println "Q      quit")
  (println "Z      undo last play")
  (println "?/H    show help")
  (println))

; Next card command

(defn- turn 
  "Turn over the next card in the pack"
  [table] 
  (let [top (:top table)
        num (count (:pack table))
        hi (dec num)]
    (if (>= top hi)
      (assoc table :top (min 2 hi))
      (assoc table :top (min (+ top cards-to-turn) hi)))))

; PLAY A CARD

; Get the selected card

(defn- get-from-pack [table]
  (let [indx (:top table)]
    (if (neg? indx) nil ((:pack table) indx))))

(defn- get-from-piles [x table]
  (let [piles (:piles table)]
    (if (or (neg? x) (>= x (count piles)) (neg? (piles x)))
      nil
      (piles x))))

(defn- get-from-layout [x y table]
  (let [layout (:layout table)
        hiddens (:layout-hidden-cards table)]
    (if (or (neg? x) (>= x (count layout)))
      nil
      (let [column (layout x)
            hidden (hiddens x)]
        (if (or (< y hidden) (>= y (count column)))
          nil
          (column y))))))
  
(defn- which-card 
  "Identify card to be played" 
  [type x y table]
  (case type
    :pack (get-from-pack table)
    :ace-pile (get-from-piles x table)
    :layout (get-from-layout x y table)
    nil))   

; Remove the played card from its old location

(defn- remove-from-pack 
  [table]
  (let [pack (:pack table)
        top (:top table)
        new-pack (com/vec-remove pack top)
        new-table1 (assoc-in table [:pack] new-pack)]
    (assoc-in new-table1 [:top] (dec top))))

(defn- remove-from-piles
  [x table]
  (let [piles (:piles table)
        card (piles x)]
    (assoc-in table [:piles x] (dec card))))
  
(defn- remove-from-layout 
  [x y table]
  (let [layout (:layout table)
        column (nth layout x)
        table1 (assoc-in table [:layout x] (subvec column 0 y))
        hiddens (:layout-hidden-cards table)
        hidden (nth hiddens x)]
    (if (and (pos? hidden) (= y hidden))
      (assoc-in table1 [:layout-hidden-cards x] (dec hidden))
      table1)))

(defn- remove-card [type x y table]
  (case type
    :pack (remove-from-pack table)
    :ace-pile (remove-from-piles x table)
    :layout (remove-from-layout x y table)
    nil))

; Playing the card
                   
(defn- play-to-piles [type x y table]
  (if (= type :ace-pile)
    nil ; Makes no sense to play pile card to itself
    (let [card (which-card type x y table)
          piles (:piles table)
          suit1 (com/suit card)
          rank1 (com/rank card)
          current-card (nth piles suit1)
          current-rank (if (neg? current-card) -1 (com/rank current-card))]
      (if (and (= rank1 (inc current-rank))
               ; Only a top layout card may be played to a pile
               (or (= type :pack) (= y (dec (count (nth (:layout table) x))))))
        (let [new-table (assoc-in table [:piles suit1] card)]
          (remove-card type x y new-table))
        nil))))

; Play to the layout

(defn- play-on-this-layout-column? [col-index layout card]
  (let [col (nth layout col-index)
        rank (com/rank card)
        color (com/color card)]
    (if (or (and (empty? col) (= rank (dec com/cards-in-suit))) ; King to empty column?
            (and (not (empty? col))
                 (= (com/rank (last col)) (inc rank))
                 (not (= (com/color (last col)) color))))
      col-index
      false))) 
  
(defn- play-to-layout [type x y table]
  (let [card (which-card type x y table)
        layout (:layout table)
        col-index (some #(play-on-this-layout-column? % layout card) (range (count layout)))]
    (if (not col-index)
      nil
      (let [column (nth layout col-index)
            new-column (if (not (= type :layout)) ; from deck or ace pile, just add the card
                         (conj column card)
                         (into column (subvec (nth layout x) y)))
            new-table (assoc-in table [:layout col-index] new-column)]
        (remove-card type x y new-table)))))  
  
(defn- parse-play-args [args-string table]
  "INPUT:
   0        play the turned card from the pack
   x y      play from layout column x (1-7), row y (1-N)
   -x       play from pile x (1-4) to layout
   
   OUTPUT:
   [:pack    n/a n/a]
   [:ace-pile x  n/a]   x is 0-3
   [:layout   x   y ]   x is 0-6, y is 0-N"
  
  (try
    (let [args (str/split args-string #" ")
          which (if (or (empty? args) (empty? (first args))) 0 (read-string (first args)))]
      (if (or (nil? which) (not (integer? which)) (< which (- suits-in-deck)) (> which com/columns-in-layout))
        [nil -1 -1]
        (let [where (if (> (count args) 1)
                      (read-string (second args))
                      (if (not (pos? which))
                        -1
                        (let [layout (:layout table)]
                          (count (nth layout (dec which))))))
              type (cond (pos? which) :layout (zero? which) :pack :else :ace-pile)
              x (cond (= type :layout) (dec which) (= type :pack) -1 :else (dec (- which)))
              y (if (= type :layout) (dec where) -1)]
          [type x y])))
    (catch Exception e [nil -1 -1])))
                  
(defn- play 
  [args table]
  "Play the card from the given position, to the ace piles or to the layout."
  (let [parms (parse-play-args args table)
        type (first parms)
        x (parms 1)
        y (parms 2)
        card (which-card type x y table)]
    (if-not card
      (com/beep table "Invalid card selection")
      (let [pile-table (play-to-piles type x y table)]
        (if pile-table
          pile-table
          (let [layout-table (play-to-layout type x y table)]
            (if layout-table
              layout-table
              (com/beep table "Could not be placed anywhere")))))))) 

; Check for game complete

(defn- check-win [table]
  (if (every? #(= (com/rank %) 12) (:piles table))
    (let [best-time (tm/best-time! table pref-file)] 
      (display/show table) 
      (println "YOU WIN !!!!!!!!")
      (when best-time (println (str "BEST TIME SET AT " best-time " SECONDS!")))
      nil)
    table))

; Auto-play feature

(defn- find-autoplay-col
  "Which column (0-6) has the lowest playable card?
   -1 means no cards remain to play.
   Thanks, Michael, for idiomatizing it."
  [layout]
  (let [col (apply min-key
                   #(or (some-> % layout last com/rank) Long/MAX_VALUE)
                   (range (count layout)))]
    (if (empty? (layout col)) -1 col)))

(defn- autoplay
  "Play from the layout, always choosing a card of minimum rank,
   until the layout is empty, or you can't play a card."
  [table]
  (loop [new-table table interim-table nil]
    (when (and (not (nil? interim-table)) (not (= interim-table table)))
      (display/show interim-table)) 
    (let [layout (:layout new-table)
          col-to-play (find-autoplay-col layout)]
      (if (not (neg? col-to-play))
        (let [col (layout col-to-play)
              row (dec (count col))
              next-table (play-to-piles :layout col-to-play row new-table)]
          (if (nil? next-table)
            (check-win new-table)
            (recur next-table new-table)))
        (check-win new-table)))))  

; Misc.

(defn- bye-bye [] (println "Good bye!")) ; returns nil

(defn- dump [table]
  (clojure.pprint/pprint table)
  table)

(defn- undo [table]
  (let [prev (undo/undo table)]
    (if prev
      prev
      (com/beep table "Can't undo before doing anything"))))

; Dispatch command

(defn- do-command [table]
  (let [input (read-line)
        command-str (if (empty? input) "z" (str input))
        command-key (str/upper-case (first command-str))
        command-data (str/trim (subs command-str 1))]
    (if-not (empty? command-str)
      (case command-key
        "N" (turn table)                 ; Turn three cards
        "P" (check-win (play command-data table))  ; Play a card
        "U" (play (str "-" command-data) table) ; Unplay from piles
        "A" (autoplay table)  ; Play out the layout
        "Q" (bye-bye)                    ; Quit the game
        "R" (deal)                       ; Re-deal
        ("?" "H") (do (help) table)      ; Help
        "D" (dump table)                 ; Dump state for debugging
        "Z" (undo table)
        (com/beep table (str "Unknown command:" command-str)))  ; other
      (com/beep table "No command"))))

; Main loop

(defn -main
  [& args]
  (help)
  (loop [table (deal)]
    (display/show table)
    (let [newtable (do-command table)]
      (when newtable (recur (undo/chain newtable table))))))

