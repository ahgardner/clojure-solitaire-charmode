(ns solitaire.common
  (:require [clojure.java.io :as io]))

; Klondike solitaire (character mode) - common

;   Specific to solitaire

(def black-suits #{0 3})
(def columns-in-layout 7)
(def cards-in-suit 13)

(defn rank [card] (if (neg? card) -1 (mod card cards-in-suit)))

(defn suit [card] (quot card cards-in-suit))

(defn color [card]
  (if (contains? black-suits (suit card)) :black :red))

;   General

(defn vec-remove
  "remove an element from a vector"
  [v pos]
  (vec (concat (subvec v 0 pos) (subvec v (inc pos)))))

(defn max-count [coll]
  "From a collection of collections,
   get the count of the largest subcollection"
  (apply max (map count coll)))
    
(defn log 
  "Write a log message" 
  [message]
  (with-open [wrtr (io/writer "solitaire.log" :append true)]
             (.write wrtr (str message (System/getProperty "line.separator")))))

(defn beep 
  "Make an error sound and return arg"
  [arg msg]
  (log msg)
  (println (char 7))
  arg)

