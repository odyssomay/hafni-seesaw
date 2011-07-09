(ns hafni-seesaw.text
  (:require [seesaw.core :as ssw])
  (:import javax.swing.text.StyleConstants))

(defn- set-attribute [jdoc style pos]
  "Set attribute with position specified in pos to doc."
  (.setCharacterAttributes jdoc (first pos) (second pos) style true))

(defn text-pane-input-arr 
  "Fields:
  :style - set the style of a part of the text, arguments are
           the style (must be present in :styles), the offset
           and the length of the change | [String Int Int]
  :styles - add styles to be available, :name must be provided, and is
            used as an identifier to :style.
            Please note that :font takes the FontFamily as a string - and not
            a java.awt.Font object.
                  | [{:name String :font String :color java.awt.Color 
                      :size Int :background java.awt.Color
                      :bold Bool :italic Bool :underline Bool}]"
  [c field]
  (case field
    :style #(let [document (.getStyledDocument c)]
              (set-attribute document (.getStyle document (first %)) (rest %)))
    :styles (fn [coll]
              (dorun (map #(let [style (.addStyle c (:name %) nil)]
                             (dorun (map (fn [x]
                                           (condp = (first x)
                                             :font (.addAttribute style StyleConstants/FontFamily (second x))
                                             :size (.addAttribute style StyleConstants/FontSize (second x))
                                             :color (.addAttribute style StyleConstants/Foreground (second x))
                                             :background (.addAttribute style StyleConstants/Background (second x))
                                             :bold (.addAttribute style StyleConstants/Bold (second x))
                                             :italic (.addAttribute style StyleConstants/Italic (second x))
                                             :underline (.addAttribute style StyleConstants/Underline (second x))
                                             nil))
                                         %)))
                          coll)))
    #(ssw/config! c field %)))

(let [event_holder (atom {})]
  (defn text-pane-event 
    "Events:

IMPORTANT:
    if you use any of these events, the document of the text pane will change!
    Also, if you change the document after you used one of the events, 
    the events will no longer have any effect.

    :inserted - text was inserted, sends the 
                offset and the inserted text | [Int String]
    :removed - text was removed, sends the offset
               and length of the removed text | [Int Int]
    :insert - ADVANCED! 
              When the user inserts text (and before it has been
              added to the area) a pair with the offset
              of the insertion and the text to insert is sent to 
              this event. The return value of this event should be
              of the same form, and will be the offset and text actually
              inserted into the area. If this event isn't connected
              the text is simply inserted as it normally would | [Int String]
    :remove - ADVANCED!
              Like :insert with the difference that the pair holds the
              offset and length of the removal. | [Int Int]"
    [c field f]
    (if (case field
          :inserted true
          :removed true
          :insert true
          :remove true
          false)
      (if (contains? @event_holder c)
        (let [a (get-in @event_holder [c field])]
          (reset! a f)
          #(reset! a nil))
        (let [inserted_f (atom nil)
              removed_f (atom nil)
              insert_f (atom nil)
              remove_f (atom nil)
              document (proxy [javax.swing.text.DefaultStyledDocument] []
                         (insertString [offset text a]
                                       (let [ins (if @insert_f
                                                   (@insert_f [offset text])
                                                   [offset text])]
                                         (proxy-super insertString (first ins) (second ins) a)
                                         (if @inserted_f
                                           (@inserted_f ins))))
                         (remove [offset length]
                                 (let [remv (if @remove_f
                                              (@remove_f [offset length])
                                              [offset length])]
                                   (proxy-super remove (first remv) (second remv))
                                   (if @removed_f
                                     (@removed_f remv)))))]
          (swap! event_holder assoc c 
                 {:inserted inserted_f
                  :removed removed_f
                  :insert insert_f
                  :remove insert_f })
          (.setDocument c document)
          (recur c field f)))
      #(ssw/listen c field f)))) ;; else clause of (if (case field ...
