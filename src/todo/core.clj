(ns todo.core2
  (:gen-class)
  (:require [seesaw.dnd :as dnd]
            [clojure.java.io :as io])
  (:use [seesaw core mig]))


(defn print-it [note arg]
  "debugging tool"
  (println "print-it. " note " " arg)
  arg)

;; io -------------------------------------------------------------------------

(def storage-folder "lists/")

(defn loaded-lists []
  " Returns a file-seq of all of the todo-lists in memory - active and hidden
    aren't sorted out at this point. This is only run at start-up and is not
    updated when lists are created/deleted - those changes are stored in the
    'organizer'. "
  (let [return-value (-> "lists/"
                         clojure.java.io/file
                         file-seq
                         rest)]
    return-value))
  

(defn get-name []
  " .getName isn't being found (not available at read-time?) so it's wrapped
    up here. "
  (fn [x] (.getName x)))

(defn get-list-names []
  " Returns a list of todo-list names in memory at the time of program
    launch. "
  (let [return-value 
        (map (get-name) (loaded-lists))]
    return-value))

(declare organizer)

(defn get-list [list-name]
  " Returns a list of todo-list items from memory for a given list name. "
  ;; storage-folder is used here instead of (:storage organizer) because
  ;; organizer isn't initializing/returning properly...
  (let [raw-file (try (slurp (str storage-folder list-name))
                      (catch Exception e))]
     (if raw-file
       (binding [*read-eval* false]  ;;  security?
         (-> raw-file
             read-string             ;; convert file to clojure list
             ;;rest                    ;; first element is "active"
             ((fn [y] (map str y)))));; convert list members to str's
       "this is an error" ;; else return an empty list
       )))

(defn active? [list-name]
  " Given a list name load the list from memory and check if it is active.  
    Returns true if active. "
  (-> list-name
      get-list
      (#(= (first %) "active"))))

(defn hidden? [list-name]
  " Given a list name load the list from memory and check if it is 'hidden'.
    Returns true if the list is tagged as hidden. "
  (-> list-name
      get-list
      (#(= (first %) "hidden"))))

(defn get-hidden-list-names []
  " Finds all of the hidden lists in memory.  Returns a list of their names. "
  (filter hidden? (get-list-names)))

(defn get-active-list-names []
  " Finds all of the hidden lists in memory.  Returns a list of their names. "
  (filter active? (get-list-names)))

(defn list-not-nil? [list-name]
  " Returns true if a given list name corresponds to at least one todo item. "
  ;;(println "list-not-nil? list-name:" list-name)
  (if (not (nil? (get-list list-name)))
    true
    false))

(declare make-todo)

(defn startup-lists []
  " Returns a list of active todo-lists from memory, initialized and ready
    for display.  Hidden lists can be initialized one at a time as needed. "
  (let [no-nil-lists (doall (filter list-not-nil? (get-active-list-names)))]
    (map #(make-todo % "none") no-nil-lists)))
      
(defn save-list! [the-list list-name]
  " Saves list data to local memory. "
    (spit (str storage-folder list-name)
          (prn-str (map str (conj the-list "active")))))

(defn delete-list! [list-name]
  ;; DANGEROUS!!!  This could use some checking
  (io/delete-file (str (:storage organizer) list-name)))


;; controller/logic -----------------------------------------------------------

(defn drop-from-atom! [a i]
  "Drop an item from within an atom, if it's there.  Will drop all atom
   members that match the given item."
  (swap! @a (fn [x] (filter #(not (= i %)) x))))

;; (defn startup-lists []
;;   [(make-todo "first one" "none")
;;    (make-todo "first one" "none")
;;    (make-todo "second one" "none")
;;    (make-todo "second one" "none")])

(declare the-frame active-content)

(def organizer
  ;; " This is a global dictionary for the things that make sense in a
  ;;   global context. "
  ;; The let form fixes a problem where the active and hidden atoms were
  ;; being initialized with nil for contents.
  (let [active-list-names (get-active-list-names)
        hidden-list-names (get-hidden-list-names)]
    {:storage "lists/"
     :active-list-names (atom active-list-names)
     :hidden-list-names (atom hidden-list-names)
     :delete-list! (fn [item]
                     " Kind of hard-core - attempts to delete the item from 
                   both hidden and active lists without checking to see
                   if it was A) there in the first place or B) if it 
                   succeeded at all.  Also attempts to delete the list from
                   memory with complete disregard for safety. "
                     (doall (drop-from-atom! (:active-lists organizer) item)
                            (drop-from-atom! (:hidden-lists organizer) item)
                            (delete-list! item)))
     :new-list! (fn [list-name]
                  (println "new list!!! with name: " list-name)
                  (save-list! '("active") list-name)
                  (swap! (:active-list-names organizer) #(conj % list-name))
                  (config! the-frame :content (active-content)))
     
     ;; :call-rebuild (fn [] "what")
     ;; :completed-list (atom nil) ;; on hold
     ;; :completed-fn (atom nil) ;; on hold
     })) 

(defn list-with-moved-element
  ;; this borrows heavily from seesaw/test/examples/reorderable_listbox
  [current-list element new-index]
  (let [current-list (vec current-list)
        current-index (.indexOf current-list element)]
    (if (= new-index current-index)
      current-list
      (if (< new-index current-index)
        (concat (subvec current-list 0 new-index)
                [element]
                (subvec current-list new-index current-index)
                (subvec current-list (inc current-index)))
        (concat (subvec current-list 0 current-index)
                (subvec current-list (inc current-index) new-index)
                [element]
                (subvec current-list new-index))))))

(defn drop-one [v i]
  " Takes a vector and returns a new vector with the item at index i dropped,
    zero-indexed. "
  (vec (concat (subvec v 0 i) (subvec v (inc i) (count v)))))

(defn drop-item [v i]
  " Takes a vector and returns a new vector with the item(s) matching i dropped
    "
  (filter #(not (= i %)) v))


;; display (with some logic )--------------------------------------------------

;; the following function is (modified) from the seesaw examples
;; (reorderable_listbox.clj)
(defn reorderable-listbox
  " A listbox of items that the user can reorder by dragging and
    dropping.  The caller provide as input an atom containing a sequence
    of immutable data values, e.g. strings.  That sequence will give the
    original order that items appear in the list.  The atom contents will
    be changed to a new sequence whenever the user modifies the order.  No
    new items are allowed to be added, nor may existing items be removed."
  [item-list-atom]
    (listbox :model @item-list-atom
             :drag-enabled? true
             :drop-mode :insert
             :transfer-handler
             (dnd/default-transfer-handler
               ;; This is how listbox reordering is handled - essentially the
               ;; item is being exported, then imported, then the list is 
               ;; updated in the import portion to reflect the new placement
               :import [dnd/string-flavor
                        (fn [{:keys [target data drop? drop-location] :as m}]
                          ;; Ignore anything dropped onto the list
                          ;; that is not in the original set of list
                          ;; items.
                          (if (and drop?
                                   (:insert? drop-location)
                                   (:index drop-location)
                                   ((set @item-list-atom) data))
                            (let [new-order (list-with-moved-element
                                              @item-list-atom data
                                              (:index drop-location))]
                              (reset! item-list-atom new-order)
                              (config! target :model new-order))))]
               :export {:actions (constantly :copy)
                        :start   (fn [c]
                                   [dnd/string-flavor (selection c)])})))

(defn make-todo [list-name catch-finished]
  " Takes a name and an initial list and builds a todo-list frame, 
    complete with button bindings.  list-name is the name of the list that
    will be displayed to the user, catch-finished is a currently unused arg
    that would take a lambda function pointing to a 'finished' list.
    Each todo-list is encapsulated in this closure except for the button 
    bindings and 5 public methods: content, name, active?, activate!, and
    deactivate! "
  ;; catch-finished must be a lambda function that takes one argument (a set)
  ;; that handles list items when removed from their list.  It's generally the
  ;; add function of the 'completed' list
  (let [the-name list-name ;; previously (str "list-" list-name)
        ;; it is assumed that the list being loaded has "active" as its head 
        the-list (atom (rest (get-list the-name)))
        the-listbox (reorderable-listbox the-list)
        the-entryfield (text "")
        the-add-button (button :text "add")
        add-fn (fn [e] ;; lambda fn to add new item from the text field (UI)
                 (let [entry-text (text the-entryfield)]
                   (if (> (count entry-text) 0)                ;; if new text
                     (do (swap! the-list #(conj % entry-text)) ;; update atom
                         (save-list! @the-list the-name)       ;; save atom
                         (text! the-entryfield "")             ;; clear textbox
                         (config! the-listbox :model @the-list))))) ;;update lbx
        _ (listen the-add-button :action add-fn) ;; bind add button to add-fn
        the-remove-button (button :text "remove")
        remove-fn (fn [e] ;; fn bound to the remove button (UI) 
                    (let [selected (set
                                    (selection the-listbox {:multi? true}))]
                      ;(println "selected: " selected)
                      (swap! the-list #(remove selected %))
                      (catch-finished selected) ;; sends to the done list
                      (save-list! @the-list the-name)
                      (config! the-listbox :model @the-list)))
        _ (listen the-remove-button :action remove-fn) 
        the-shuffle-button (button :text "shuffle")
        _ (listen the-shuffle-button :action
                  (fn [e]
                    (swap! the-list shuffle)
                    (config! the-listbox :model @the-list)
                    (save-list! @the-list the-name)))
        ;; functionality's done, now we lay out the interface
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
        ;; the active atom allows lists to be hidden without being deleted
        active (atom true)]
    {:content the-content
     :name the-name
     :active? (deref active)
     :activate! #(reset! active true)
     :deactivate! #(reset! active false)}))

(defn display-lists [frames]
  " This is what determines how the todo lists will be tiled. This could maybe
    be more dynamic? "
  ;(println "hih?")
  ;(println "\nin display-lists, frames:\n" frames "\n")
  (let [the-count (count frames)
        the-content (map :content frames)]
    ;(println "\nthe-content:" the-content)
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



(def modify-button
  ;; There is one modify button that allows the todo lists to be rearranged
  (button :text "modify"))

(listen modify-button
        ;; pop-up a new window for meta list options.  Everything for the
        ;; 'modify' pop-up is encapsulated in this closure.  This is a bit 
        ;; complicated and maybe should be broken out into pieces, as it is
        ;; it wouldn't really fit into the usual 80 columns
        
        ;; needs a "done" button
        ;; needs an "add new active list" button
        
        :action (fn [e]
                  (let [;; active-listbox (listbox :model
                        ;;                        ["one" "two" "three"])
                        active-model (:active-list-names organizer)
                        active-listbox (reorderable-listbox active-model)
                        hidden-model (:hidden-list-names organizer)
                        hidden-listbox (reorderable-listbox hidden-model)
                        make-hidden (button :text "make selected hidden")
                        make-active (button :text "make selected active")
                        delete-list (button :text "delete selected hidden list")
                        add-new-list (button :text "add new active list")
                        ;; done-button (button :text "done") ;; future 
                        _ (listen make-hidden :action
                                  ;; "make-hidden moves a string from the active
                                  ;;  list to the hidden list.  It does not happen
                                  ;;  in a transaction and it's a multistep process
                                  ;;  so there's a possibility of state problems."
                                  (fn [e]
                                    (let [mover (selection active-listbox)]
                                      (swap! hidden-model #(conj % mover))
                                      (swap! active-model #(drop-item % mover))
                                      (config! hidden-listbox :model @hidden-model)
                                      (config! active-listbox :model @active-model))))
                        _ (listen make-active :action
                                  ;; "See notes for make-hidden above.
                                  ;;  They are almost identical."
                                  (fn [e] (let [mover (selection hidden-listbox)]
                                      (swap! active-model #(conj % mover))
                                      (swap! hidden-model #(drop-item % mover))
                                      (config! hidden-listbox :model @hidden-model)
                                      (config! active-listbox :model @active-model))))
                        delete-dialog (dialog :content
                                              (flow-panel :items
                                                          ["Delete the selected todo-list from the second list box? This will delete the list and all of its items and this can't be undone!"])
                                              :option-type :ok-cancel
                                              :success-fn (fn [e]
                                                            (let [deletion (selection hidden-listbox)]
                                                              (swap! hidden-model #(drop-item % deletion))
                                                              (config! hidden-listbox :model @hidden-model)
                                                              )))
                        _ (listen delete-list :action (fn [e]
                                                        (-> delete-dialog
                                                            pack!
                                                            show!)))
                        entry-field (text "add new name here")
                        add-new-dialog (dialog :content
                                                 (flow-panel :items
                                                             ["add new todo-list with name: " entry-field])
                                                 :option-type :ok-cancel
                                                 :success-fn (fn [e]
                                                               (let [new-name (text entry-field)]
                                                                 (swap! active-model #(conj % new-name))
                                                                 (config! active-listbox :model @active-model)
                                                                 ((:new-list! organizer) new-name)
                                                                 ;; add to display
                                                                 true)))
                        _ (listen add-new-list :action (fn [e]
                                                         (-> add-new-dialog
                                                             pack!
                                                             show!)))]
                    (-> (frame :title "manage todo lists"
                               :content
                               (top-bottom-split
                                (border-panel :north "Active Todo Lists"
                                              :center active-listbox
                                              :south (left-right-split make-hidden add-new-list))
                                (border-panel :north "Hidden Todo Lists"
                                              :center hidden-listbox
                                              :south (left-right-split make-active delete-list))
                                :divider-location 1/2))
                        pack!
                        show!))))
                                
(defn active-content []
  ;(println "in active-content, startup-lists:" (startup-lists))
  (border-panel
   :center (display-lists (startup-lists))
   :south modify-button))

(def the-frame
  ;;(println "active-content: " (active-content)))
  (frame :title "my todo list(s)"
         :content (active-content)
         :width 650
         :height 650
         :on-close :exit))
  
(defn -main  [& args]
  (native!)
  (-> the-frame
      pack!
      show!))
