(ns solitaire.display
  (:require [solitaire.common :as com]))

; Klondike solitaire (character mode) - display

(def rank-symbols ["A" "2" "3" "4" "5" "6" "7" "8" "9" "X" "J" "Q" "K"]);
(def suit-symbols [\u2660 \u2665 \u2666 \u2663]) ; ♠ ♥ ♦ ♣
(def ANSI_RED "\u001B[31m")
(def ANSI_RESET "\u001B[0m")

(defn card-symbol
  "Return a string for displaying the card"
  [card]
  (if (neg? card)
    "--"
    (let [display (str (nth rank-symbols (com/rank card)) (nth suit-symbols (com/suit card)))]
      (if (= (com/color card) :red)
        (str ANSI_RED display ANSI_RESET)
        display))))

(defn show [table]
  
  ; Last card played from the deck
  (let [top (:top table)
        pack (:pack table)]
    (println (if (< top 0) "--" 
               (card-symbol (nth pack top)))))
  
  ; The 4 suit piles
  (println (apply str
                  (for [x (:piles table)] (str (card-symbol x) " "))))
  (println)
  
  ; The layout
  (let [layout (:layout table)
        hidden-cards (:layout-hidden-cards table)
        max-column (com/max-count layout)
        display-rows
        (for [row (range max-column)]
          (apply str
                 (for [col (range com/columns-in-layout)
                       :let [column (nth layout col)
                             height (count column)
                             card (if (>= row height) -1 (nth column row))
                             hidden (nth hidden-cards col)]]
                   (if (>= row height)
                     "   "
                     (if (< row hidden)
                       "[] "
                       (str (card-symbol card) " "))))))]
    
    (doseq [i (range (count display-rows))]
      (println (nth display-rows i))))
  
  (flush)(Thread/sleep 200))
