(ns solitaire.time (:require [clojure.java.io :as io] 
                             [clj-time.core :as t]
                             [clojure.string :as str]))

; Klondike solitaire (character mode) - track best time

; This is the first time I've added a feature I don't believe in or enjoy.
; I just added "best time" for more Clojure practice.

(def start-time-key :solitaire-start-time)

(def pref-file "solitaire.prefs")

(defn- get-time []
  (let [file-exists (.exists (io/as-file pref-file))]
    (if file-exists
      (let [hsstr (slurp pref-file)]
        (read-string (second (str/split hsstr #"="))))
      10000)))

(defn- set-time! [new-time]
      (spit pref-file (str "best-time=" new-time)))

(defn- diff [old-time new-time] 
  (t/in-seconds (t/interval old-time new-time)))

(defn start [m] (assoc m start-time-key (t/now)))

(defn best-time! [m] ; return nil if not a best time
  (let [start-time (start-time-key m)
        end-time (t/now)
        new-time (diff start-time end-time)
        old-time (get-time)]
    (when (< new-time old-time)
      (set-time! new-time)
      new-time)))

