(ns hafni-seesaw.core
  (:use clj-arrow.arrow
        (hafni-seesaw 
          [container :only [combo-box-input-arr
                            list-input-arr
                            tabbed-pane-input-arr]]
          [menu :only [menu-input-arr]]
          [meta :only [put-meta! get-meta]]
          [text :only [text-pane-input-arr text-pane-event]]
          [tree :only [tree-input-arr tree-event]]))
  (:require [clojure.tools.logging :as log]
            (seesaw [core :as ssw]
                    [meta :as ssw-meta])))

(defn input-arr [c field]
  (>>>
    (&&& 
      (fn [value] (put-meta! c field value))
      (condp #(isa? (class %2) %1) c
        ;; text
        javax.swing.JTextPane (text-pane-input-arr c field)
        ;; tree
        javax.swing.JTree (tree-input-arr c field) 
        ;; container
        javax.swing.JComboBox (combo-box-input-arr c field)
        ;        javax.swing.JList (list-input-arr c field)
        javax.swing.JTabbedPane (tabbed-pane-input-arr c field)
        ;; menu
        javax.swing.JMenu (menu-input-arr c field)
        ;; otherwise
        #(ssw/config! c field %)))
    second))

(defn output-arr [c field]
  (fn [_]
    (get-meta c field)))


(defn listen 
  ([c field f]
   (condp #(isa? (class %2) %1) c
;; text
     javax.swing.JTextPane (text-pane-event c field f)
;; tree
     javax.swing.JTree (tree-event c field f)
;; otherwise
     (ssw/listen c field f)))
  ([c field f & fields+fs]
   (let [l1 (listen c field f)
         l2 (apply listen c fields+fs)]
     #(do (l1)
        (l2)))))

(defn config!
  ([c field value]
   ((input-arr c field) value))
  ([c field value & fields+values]
   {:pre [(even? (count fields+values))]}
   (config! c field value)
   (apply config! c fields+values)))

