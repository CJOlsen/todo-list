;; todo list
;; Author: Christopher Olsen
;; Copyright 2013
;; License: GPLv3 (a copy should be included, if not visit 
;;          http://www.gnu.org/licenses/gpl.txt

(ns todo.core
  (:gen-class)
  (:use [seesaw core mig]))

(import java.util.LinkedList) ; mutable list

;;
;; persistence
;;

(defn save-list
  [the-list]
  (spit "list-data" (prn-str (map str the-list))))

(defn get-list
  []
  ;; try to load current list data, if that fails initialize with an empty
  ;; mutable java list
  (let [raw-file (try (slurp "list-data")
                      (catch Exception e))]
    (if raw-file
      (binding [*read-eval* false]
                (-> raw-file
                    read-string             ;; convert file to clojure list
                    ((fn [y] (map str y)))  ;; convert list members to strings
                    java.util.LinkedList.))  ;; convert to mutable java type
      (java.util.LinkedList. '()))))
    
(def lb-list (get-list)) ; this is the holder of the lb data

;;
;; interface
;;

(def todo-listbox
  (listbox :model lb-list)) ; the listbox itself

(def new-entryfield
  (text "")) 

(def add-button
  (button :text "add")) 

(listen add-button :action
        (fn [e]
          (.add lb-list (text new-entryfield))
          (save-list lb-list)
          (text! new-entryfield "")
          (config! todo-listbox :model lb-list)))

(def remove-button
  (button :text "remove selected"))

(listen remove-button :action
        (fn [e]
          ; map is lazy, doall forces it to run *now*
          (doall (map (fn [item] (.remove lb-list item))
                      (selection todo-listbox {:multi? true})))
          (save-list lb-list)
          (config! todo-listbox :model lb-list )))

(def shuffle-button
  (button :text "shuffle"))

(listen shuffle-button :action
  (fn [e]
    ;; overwrite lb-list (not ideal)
    (def lb-list (java.util.LinkedList. (into () (shuffle lb-list))))
    (config! todo-listbox :model lb-list)))

(def entry-split
  (left-right-split new-entryfield
                    add-button
                    :divider-location 2/3))

(def south-split
  (left-right-split remove-button
                    shuffle-button
                    :divider-location 2/3))

(defn make-content []
  (border-panel
   :north entry-split
   :center (scrollable todo-listbox)
   :south south-split
   :vgap 5 :hgap 5 :border 5))


(defn -main [& args]
  (native!)
  (-> (frame :title "my todo list"
             :content (make-content)
             :width 350
             :height 250
             :on-close :exit)
      show!))
