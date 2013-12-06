(ns todo.core2
  (:gen-class)
  (:use [seesaw core mig]))

;; save
;; load
;; store list
;; remove item from list
;; add to list
;; shuffle list

(defn save-list [the-list list-name]
  """ Saves list data to local memory."""
    (spit (str "lists/" list-name) (prn-str (map str the-list))))

(defn get-list [list-name]
  ;; try to load current list data, if that fails initialize with an empty
  ;; mutable java list
  (let [raw-file (try (slurp (str "lists/" list-name))
                      (catch Exception e))]
    (if raw-file
      (binding [*read-eval* false]  ;; was this for security?
                (-> raw-file
                    read-string             ;; convert file to clojure list
                    ((fn [y] (map str y)))));; convert list members to str's
      '())))

;; what follows is a gigantic closure....

(defn make-todo [list-name]
  """ Takes a name and an initial list and builds a todo-list frame, 
      complete with button bindings """
  (let [the-name (str "list-" list-name)
        the-list (atom (get-list the-name))
        the-listbox (listbox :model @the-list)
        the-entryfield (text "")
        the-add-button (button :text "add")
        _ (listen the-add-button :action
                  (fn [e]
                    (swap! the-list #(conj % (text the-entryfield)))
                    (save-list @the-list the-name)
                    (text! the-entryfield "")
                    (config! the-listbox :model @the-list)))
        the-remove-button (button :text "remove")
        _ (listen the-remove-button :action
                  (fn [e]
                    (swap! the-list
                           #(remove
                             (set (selection the-listbox {:multi? true}))
                             %))
                    (save-list @the-list the-name)
                    (config! the-listbox :model @the-list)))
        the-shuffle-button (button :text "shuffle")
        _ (listen the-shuffle-button :action
                  (fn [e]
                    (swap! the-list shuffle)
                    (config! the-listbox :model @the-list)))
        the-north-split (top-bottom-split
                         (label list-name)
                         (left-right-split the-entryfield
                                          the-add-button
                                          :divider-location 2/3))
        the-south-split (left-right-split the-remove-button
                                          the-shuffle-button
                                          :divider-location 2/3)
        the-content (border-panel
                     :north the-north-split
                     :center (scrollable the-listbox)
                     :south the-south-split
                     :vgap 5 :hgap 5 :border 5)]
    {:content the-content
     :name the-name}))


(defn display-lists [frames]
  (let [the-count (count frames)]
    (cond (= the-count 1) ()
          (= the-count 2) ()
          (= the-count 3) ()
          (= the-count 4) ()
          (= the-count 5) ()
          (= the-count 6) ()
          (= the-count 7) ()
          (= the-count 8) ()
          (= the-count 9) ()
)))

(def todo-lists (map make-todo ["first one" "second one"]))

(def test-content (left-right-split (:content (first todo-lists))
                                    (:content (first (rest todo-lists)))
                                    :divider-location 1/2))

(def test-todo (make-todo "a test"))

(defn -main  [& args]
  (native!)
  (-> (frame :title "my todo list(s)"
             :content test-content
             :width 650
             :height 250
             :on-close :exit)
      pack!
      show!))
