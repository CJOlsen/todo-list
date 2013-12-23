(ns todo.core2
  (:gen-class)
  (:use [seesaw core mig]))


(def temp-shuffle-button
  (button :text "shuffle!!!"))
(declare organizer)
(listen temp-shuffle-button :action #(doall
                                      ((:shuffle-lists organizer))
                                      ((:call-rebuild organizer))))

(declare test-content)
(def organizer
  ;""" This is a global dictionary for the few things that make sense in a global
  ;    global context. """
  {:storage "lists/"
   :active-lists (atom [])
   :hidden-lists (atom [])
   :make-hidden (fn [_name] nil)
   :make-active (fn [_name] nil)
   :move-right (fn [_name] nil)
   :shuffle-lists (fn [] (swap! (:active-lists organizer) shuffle))
   :call-rebuild (fn [] "what")
   :completed-list (atom nil) ;; on hold
   :completed-fn (atom nil) ;; on hold
   }) 

(defn save-list [the-list list-name]
  """ Saves list data to local memory."""
    (spit (str (:storage organizer) list-name) (prn-str (map str the-list))))

(defn get-list [list-name]
  ;; try to load current list data
  (let [raw-file (try (slurp (str (:storage organizer) list-name))
                      (catch Exception e))]
    (if raw-file
      (binding [*read-eval* false]  ;; was this for security?
                (-> raw-file
                    read-string             ;; convert file to clojure list
                    ((fn [y] (map str y)))));; convert list members to str's
      '())))


;; what follows is a(n absurdly) gigantic closure....

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
        add-fn (fn [e] ;; lambda fn to add new item from the text field
                 (let [entry-text (text the-entryfield)]
                   (if (> 0 (count entry-text))
                     (doall (swap! the-list #(conj % entry-text))
                            (save-list @the-list the-name)
                            (text! the-entryfield "")
                            (config! the-listbox :model @the-list)))))
        _ (listen the-add-button :action add-fn) ;; bind add button to add-fn
        the-remove-button (button :text "remove")
        remove-fn (fn [e]
                    (let [selected (set
                                    (selection the-listbox {:multi? true}))]
                      (println "selected: " selected)
                      (swap! the-list #(remove selected %))
                      (catch-finished selected) ;; sends to the done list
                      (save-list @the-list the-name)
                      (config! the-listbox :model @the-list)))
        _ (listen the-remove-button :action remove-fn) 
        the-shuffle-button (button :text "shuffle")
        _ (listen the-shuffle-button :action
                  (fn [e]
                    (swap! the-list shuffle)
                    (config! the-listbox :model @the-list)
                    (save-list @the-list the-name)))
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
     :deactivate! #(reset! active false)}))


;; ;; this is on hold for now, probably requires a new add-fn added to make-todo
;; ;; that will handle sets of strings instead of strings
;; (defn make-completed-list []
;;   (reset! (:completed-list organizer)
;;           (make-todo "completed" "none")))

;; (make-completed-list)



(defn display-lists [frames]
  (let [the-count (count frames)
        the-content (map :content frames)]
    (cond (= the-count 1) (nth the-content 0)
          (= the-count 2) (left-right-split (nth the-content 0)
                                            (nth the-content 1)
                                            :divider-location 1/2)
          (= the-count 3) (top-bottom-split
                           (left-right-split (nth the-content 0)
                                             (nth the-content 1)
                                             :divider-location 1/2)
                           (nth the-content 2) :divider-location 1/2)
          (= the-count 4) (top-bottom-split
                           (left-right-split (nth the-content 0)
                                             (nth the-content 1)
                                             :divider-location 1/2)
                           (left-right-split (nth the-content 2)
                                             (nth the-content 3)
                                             :divider-location 1/2)
                           :divider-location 1/2)
          (= the-count 5) () ;; two top three bottom
          (= the-count 6) () ;; 3+3
          (= the-count 7) () ;; 2+2+3
          (= the-count 8) () ;; 2+3+3
          (= the-count 9) () ;; 3x3
)))


;; (defn get-lists []
;;     (-> "lists/"
;;         clojure.java.io/file
;;         file-seq
;;         rest
;;         (#(map get-name %))))
;;         (fn [x] (map #(make-todo % "none")))

(def todo-lists [(make-todo "first one" "none")
                 (make-todo "first one" "none")
                 (make-todo "second one" "none")
                 (make-todo "second one" "none")])

(def mod-button
  (button :text "modify"))

(listen mod-button
        ;;Create and display a dialog to rearrange the main frame
        :action (fn [e]
                  (let [active-listbox (listbox :model
                                                ["one" "two" "three"])
                        make-hidden (button :text "make selected hidden")
                        hidden-listbox (listbox :model
                                                ["a" "b" "c"])
                        make-active (button :text "make selected active")
                        delete-list (button :text "delete selected list")
                        _ (listen make-hidden :action
                                  (fn [e] ;; get selected
                                    ;; remove from active
                                    ;; add to hidden
                                    ;; save state
                                    true))
                        _ (listen make-active :action (fn [e] true))
                        delete-dialog (dialog :content
                                              (flow-panel :items
                                                          ["Delete the todo list named: " "?"])
                                              :option-type :ok-cancel
                                              :success-fn (fn [e] (+ 5 5)))
                        _ (listen delete-list :action (fn [e] true))]
                    (-> (frame :title "manage todo lists"
                               :content
                               (top-bottom-split
                                (border-panel :north "Active Todo Lists"
                                              :center active-listbox
                                              :south make-hidden)
                                (border-panel :north "Hidden Todo Lists"
                                              :center hidden-listbox
                                              :south make-active)
                                :divider-location 1/2))
                        pack!
                        show!))))
                                


(def test-content
  (border-panel
   :center (display-lists todo-lists)
   :south mod-button))

;; r (remove)
;; s (shuffle)
;; -> (move right)
;; h (hide)

(def the-frame
  (frame :title "my todo list(s)"
         :content test-content
         :width 650
         :height 650
         :on-close :exit))

(defn -main  [& args]
  (native!)
  (-> the-frame
      pack!
      show!))
