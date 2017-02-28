(ns solitaire.undo)

; Klondike solitaire (character mode) - undo feature for a map

(def undo-key :solitaire-undo)
(def prev-key :solitaire-undo-prev)

(defn undo [m]
  (let [p (prev-key m)]
    (if p (assoc p undo-key true) p)))

(defn chain [this last]
  (if (= this last)
    this
    (if (undo-key this)
      (assoc this undo-key false)
      (assoc this prev-key last))))