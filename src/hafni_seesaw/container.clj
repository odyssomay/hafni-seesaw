(ns hafni-seesaw.container
  (:use 
    (hafni-seesaw 
      [utils :only [drop-nth]]
      [meta :only [put-meta! get-meta]])
    [clj-diff.core :only [diff]])
  (:require
    [seesaw.core :as ssw]))

(defn change-container-content
  "Change the content of a container,
  insert_f - a function of 2 arguments: the index, and the object to add
  remove_f - a function of 1 arguments: the index to remove

  returns a function that is called with the new items, the container
  will be changed to match the new items."
  [c insert_f remove_f]
  (fn [new_items]
    (let [last_items (get-meta c ::container-content)
          d (diff last_items new_items)
          with_removed (reduce #(drop-nth %2 %1) last_items (reverse (:- d)))
          with_added (diff with_removed new_items)]
      (dorun (map remove_f (reverse (:- d))))
      (dorun (map (fn [xs]
                    (let [index (inc (first xs))
                          items (rest xs)]
                      (dorun (map #(insert_f index %) (reverse items))))) (:+ with_added))))
    (put-meta! c ::container-content new_items)) )

(defn combo-box-input-arr 
  "Fields:
  :content"
  [c field]
  (case field
    :content (change-container-content c #(.insertItemAt c %2 %1) #(.removeItemAt c %))
    #(ssw/config! c field %)))

;; add function doesn't work
(defn list-input-arr 
  "Fields:
  :content"
  [c field]
  (case field
    :content (change-container-content c #(.add c (ssw/to-widget %2) (int %1)) #(.remove c (int %)))
    #(ssw/config! c field %)))

(defn tabbed-pane-input-arr 
  "Fields:
  :content - content of the tabbed pane, each entry should
             be a map in which no options are necessary.
             | [{:content Component :title String :icon Component :tip String}]"
  [c field]
  (case field
    :content (change-container-content c
               (fn [index view]
                 (.insertTab c (:title view) (ssw/to-widget (:icon view)) (ssw/to-widget (:content view)) (:tip view) index))
               #(.removeTabAt c %))
    #(ssw/config! c field %)))
