(ns solitaire.time (:require [clojure.java.io :as io] 
                             [clj-time.core :as t]
                             [clojure.string :as str]))

; Klondike solitaire (character mode) - track best time

; This is the first time I've added a feature I don't believe in or enjoy -
; I just added "best time" for more Clojure practice.

(def pref-file "solitaire.prefs")

(defn- get-time []
  (let [file-exists (.exists (io/as-file pref-file))]
    (if file-exists
      (let [hsstr (slurp pref-file)]
        (read-string (second (str/split hsstr #"="))))
      0)))

(defn- set-time! [new-time]
      (spit pref-file (str "best-time=" new-time)))

(defn best-time! [new-time]
  (let [old-time (get-time)]
    (when (< new-time old-time) (set-time! new-time))
    (min old-time new-time)))

(defn now [] (t/now))

(defn diff [old-time new-time] 
  (t/in-seconds (t/interval old-time new-time)))