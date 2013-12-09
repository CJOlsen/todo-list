(ns todo.core2
  (:gen-class)
  (:use [seesaw core mig]))

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

(defn make-todo [list-name catch-finished]
  """ Takes a name and an initial list and builds a todo-list frame, 
      complete with button bindings """
  ;; catch-finished must be a lambda function that takes one argument (a set)
  ;; that handles list items when removed from their list.  It's generally the
  ;; add function of the 'completed' list
  (let [the-name (str "list-" list-name)
        the-list (atom (get-list the-name))
        the-listbox (listbox :model @the-list)
        the-entryfield (text "")
        the-add-button (button :text "add")
        ;;;; shit.  this needs to handle both a string and a set of strings....
        ;; add-fn (fn [e] ;; lambda fn to add new item to the list
        ;;          (swap! the-list #(conj % (text the-entryfield)))
        ;;          (save-list @the-list the-name)
        ;;          (text! the-entryfield "")
        ;;          (config! the-listbox :model @the-list))
        add-fn (fn [e] false)
        _ (listen the-add-button :action add-fn) ;; bind add button to add-fn
        the-remove-button (button :text "remove")
        _ (listen the-remove-button :action
                  (fn [e]
                    (let [selected (set
                                    (selection the-listbox {:multi? true}))]
                      (println "selected: " selected)
                      (swap! the-list #(remove selected %))
                      (catch-finished selected) ;; sends to the done list
                      (save-list @the-list the-name)
                      (config! the-listbox :model @the-list))))
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
                     :vgap 5 :hgap 5 :border 5)
        active (atom true)]
    {:content the-content
     :name the-name
     :active? (deref active)
     :activate! #(reset! active true)
     :deactivate! #(reset! active false)
     :add-item add-fn}))


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
