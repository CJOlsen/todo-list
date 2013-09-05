;; todo list
;; Author: Christopher Olsen
;; Copyright 2013
;; License: GPLv3 (a copy should be included, if not visit 
;;          http://www.gnu.org/licenses/gpl.txt

(ns todo.core
  (:gen-class)
  (:use [seesaw core mig]))

(import java.util.LinkedList) ; mutable list
(def lb-list (java.util.LinkedList. '())) ; this is the holder of the lb data
(def todo-listbox (listbox :model lb-list)) ; the listbox itself
(def new-entryfield (text "")) 
(def add-button (button :text "add")) 
(listen add-button :action
        (fn [e]
          (.add lb-list (text new-entryfield))
          (text! new-entryfield "")
          (config! todo-listbox :model lb-list)))
(def rm-button (button :text "remove selected"))
(listen rm-button :action
        (fn [e]
          ; map is lazy, doall forces it to run *now*
          (doall (map (fn [item] (.remove lb-list item))
                      (selection todo-listbox {:multi? true})))
          (config! todo-listbox :model lb-list )))

(defn make-content []
  ; split pane of new-text field and and a button to add it to the listbox
  (def new-split (left-right-split new-entryfield
                                   add-button
                                   :divider-location 2/3))
  (border-panel
   :north new-split
   :center (scrollable todo-listbox)
   :south rm-button
   :vgap 5 :hgap 5 :border 5))

(defn -main [& args]
  (native!)
  (-> (frame :title "my todo list"
             :content (make-content)
             :width 350
             :height 250
             :on-close :exit)
      show!))