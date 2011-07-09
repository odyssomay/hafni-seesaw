(ns hafni-seesaw.tree
  (:use (hafni-seesaw 
          [utils :only [drop-nth find-i]]))
  (:require [seesaw.core :as ssw]))

;; model

;; this is needed for the toString method,
;; which makes JTree print the correct text

(defrecord node [root child]
  Object
  (toString [this]
            (:root this)))

(defrecord tree-model [root listeners]
  javax.swing.tree.TreeModel
  (addTreeModelListener [this l]
                        (swap! (:listeners this) conj l))
  (getChild [this parent index]
            (nth (:child parent) index))
  (getChildCount [this parent]
                 (count (:child parent)))
  (getIndexOfChild [this parent child]
                   (find-i child (:child parent)))
  (getRoot [this]
           @(:root this))
  (isLeaf [this node]
          (empty? (:child node)))
  (removeTreeModelListener [this l]
                           (if-let [index (find-i l @(:listeners this))]
                             (swap! (:listeners this) #(drop-nth index %))))
  (valueForPathChanged [this path new_value]))

;; utils

(defn- map-to-nodes [m]
  (node. (:root m) (map map-to-nodes (:child m))))

;; TODO: 
;; only fire changed structure where
;; it actually changed

(defn- fire-tree-structure-changed [model]
  (let [root @(:root model)
        e (javax.swing.event.TreeModelEvent. root (into-array [root]))]
    (dorun (map #(.treeStructureChanged % e) @(:listeners model)))))

(defn- tree-path-to-seq [path]
  (->> (.getPath path)
    (map :root)))

;; API

(let [tree_cont (atom {})]
  (defn tree-input-arr 
    "Fields:
IMPORTANT: If you use the :content field, the model of the tree will change!

    :content - a map with nodes represented as 
               {:root String :child [{:root String :child [...]}]} | Map"
    [c field]
    (case field
      :content
      (if (contains? @tree_cont c)
        #(swap! (get @tree_cont c) (constantly (map-to-nodes %)))
        (let [nodes (atom (node. nil nil))
              model (tree-model. nodes (atom []))]
          (.setModel c model)
          (add-watch nodes nil (fn [& _] (fire-tree-structure-changed model)))
          (swap! tree_cont assoc c nodes)
          (recur c field)))
      #(ssw/config! c field %))))

(defn tree-event 
  ":selected - when the user changes the selection,
  this event sends the current selection | [[String]]"
  [tree field f] 
  (case field
    :selected (let [currently_selected (atom [])
                    selection_listener 
                    (reify javax.swing.event.TreeSelectionListener
                      (valueChanged [this e]
                                    (let [paths (.getPaths e)
                                          added (filter #(.isAddedPath e %) paths)
                                          removed (remove #(.isAddedPath e %) paths)]
                                      (dorun (map (fn [r]
                                                    (swap! currently_selected
                                                           #(drop-nth (find-i r %) %))) removed))
                                      (swap! currently_selected concat added)
                                      (->> @currently_selected
                                        (map tree-path-to-seq)
                                        f))))]
                (.addTreeSelectionListener tree selection_listener)
                #(.removeTreeSelectionListener tree selection_listener))))
