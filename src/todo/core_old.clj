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

(defn save-list [the-list which-list]
  """ Saves list data to local memory.  Lists are either :45 or :15"
  (if (= which-list :45)
    (spit "list-data-45" (prn-str (map str the-list)))
    (spit "list-data-15" (prn-str (map str the-list)))))

(defn get-list [which-list]
  ;; try to load current list data, if that fails initialize with an empty
  ;; mutable java list
  (let [list-name (str "list-data-" (name which-list))
        raw-file (try (slurp list-name)
                      (catch Exception e))]
    (if raw-file
      (binding [*read-eval* false]
                (-> raw-file
                    read-string             ;; convert file to clojure list
                    ((fn [y] (map str y)))  ;; convert list members to strings
                    java.util.LinkedList.))  ;; convert to mutable java type
      (java.util.LinkedList. '()))))
    
(def lb-list-45 (get-list :45)) ; this is the holder of the lb data
(def lb-list-15 (get-list :15)) ; this is the holder of the lb data

;;
;; interface
;;

(def todo-listbox-45
  (listbox :model lb-list-45)) ; the listbox itself
(def todo-listbox-15
  (listbox :model lb-list-15))


(def new-entryfield-45
  (text "")) 
(def new-entryfield-15
  (text "")) 


(def add-button-45
  (button :text "add/45")) 
(def add-button-15
  (button :text "add/15")) 

(listen add-button-45 :action
        (fn [e]
          (.add lb-list-45 (text new-entryfield-45))
          (save-list lb-list-45 :45)
          (text! new-entryfield-45 "")
          (config! todo-listbox-45 :model lb-list-45)))
(listen add-button-15 :action
        (fn [e]
          (.add lb-list-15 (text new-entryfield-15))
          (save-list lb-list-15 :15)
          (text! new-entryfield-15 "")
          (config! todo-listbox-15 :model lb-list-15)))

(def remove-button-45
  (button :text "remove selected"))
(def remove-button-15
  (button :text "remove selected"))

(listen remove-button-45 :action
        (fn [e]
          ; map is lazy, doall forces it to run *now*
          (doall (map (fn [item] (.remove lb-list-45 item))
                      (selection todo-listbox-45 {:multi? true})))
          (save-list lb-list-45 :45)
          (config! todo-listbox-45 :model lb-list-45)))
(listen remove-button-15 :action
        (fn [e]
          ; map is lazy, doall forces it to run *now*
          (doall (map (fn [item] (.remove lb-list-15 item))
                      (selection todo-listbox-15 {:multi? true})))
          (save-list lb-list-15 :15)
          (config! todo-listbox-15 :model lb-list-15)))


(def shuffle-button-45
  (button :text "shuffle"))
(def shuffle-button-15
  (button :text "shuffle"))

(listen shuffle-button-45 :action
  (fn [e]
    ;; overwrite lb-list (not ideal)
    (def lb-list-45 (java.util.LinkedList. (into () (shuffle lb-list-45))))
    (config! todo-listbox-45 :model lb-list-45)))
(listen shuffle-button-15 :action
  (fn [e]
    ;; overwrite lb-list (not ideal)
    (def lb-list-15 (java.util.LinkedList. (into () (shuffle lb-list-15))))
    (config! todo-listbox-15 :model lb-list-15)))

(def entry-split-45
  (left-right-split new-entryfield-45
                    add-button-45
                    :divider-location 2/3))

(def south-split-45
  (left-right-split remove-button-45
                    shuffle-button-45
                    :divider-location 2/3))

(def entry-split-15
  (left-right-split new-entryfield-15
                    add-button-15
                    :divider-location 2/3))

(def south-split-15
  (left-right-split remove-button-15
                    shuffle-button-15
                    :divider-location 2/3))

(defn make-content []
  (left-right-split (border-panel
                     :north entry-split-45
                     :center (scrollable todo-listbox-45)
                     :south south-split-45
                     :vgap 5 :hgap 5 :border 5)
                    (border-panel
                     :north entry-split-15
                     :center (scrollable todo-listbox-15)
                     :south south-split-15
                     :vgap 5 :hgap 5 :border 5)
                    :divider-location 1/2))


(defn -main [& args]
  (native!)
  (-> (frame :title "my 45/15 todo list"
             :content (make-content)
             :width 650
             :height 250
             :on-close :exit)
      pack!
      show!))
