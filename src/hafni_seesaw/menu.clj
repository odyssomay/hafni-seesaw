(ns hafni-seesaw.menu
  (:use (hafni-seesaw 
          [container :only [change-container-content]]))
  (:require [seesaw.core :as ssw]))

(defn- change-menu-content [m]
  (change-container-content m
                            #(if (= %2 [])
                               (.insertSeparator m %1)
                               (.insert m (ssw/to-widget %2) %1))
                            #(.remove m %)))

(defn menu-input-arr [c field]
  (case field
    :content (change-menu-content c)
    #(ssw/config! c field %)))
