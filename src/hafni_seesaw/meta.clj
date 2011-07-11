(ns hafni-seesaw.meta
  (:require [seesaw.meta :as ssw-meta]
            [clojure.tools.logging :as log]))

(defn put-meta! [c field value]
  (if-let [m (ssw-meta/get-meta c ::hafni-seesaw)]
    (do (swap! m assoc field value)
        value)
    (do (ssw-meta/put-meta! c ::hafni-seesaw (atom {}))
        (recur c field value))))

(defn get-meta [c field]
  (if-let [m (ssw-meta/get-meta c ::hafni-seesaw)]
    (get @m field)))

