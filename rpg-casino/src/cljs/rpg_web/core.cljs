(ns rpg-web.core
  (:require
   [reagent.core :as reagent]
   [historian.core :as hist]
   [clojure.math.combinatorics :as combi]
   [cljs.reader :as read]
;   [clojure.math.numeric-tower :as math]
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

(defonce app-state
  (reagent/atom {:playx 2 :playy 2 :hp 10}))

(hist/record! app-state :app-state)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page

(defn random-state
  "Accepts a map mapping a state to the frequency that it will be chosen.
  Assumes that the frequencies add up to 100%. Will have undesired behavior if that isn't the case."
  [state-frequency-map]
  (let [r (rand)]
    (loop [[[state prob] & rest-states] (seq state-frequency-map)
           prob-sum 0
           last-sum 0]

      (let [new-sum (+ prob-sum prob)]
        (if (<= last-sum r new-sum)
          state
          (recur rest-states new-sum prob-sum))))))


(defn mezo [id]
  (let [
    effect (random-state {1 0.4, 2 0.3, 3 0.2, 4 0.05, 10 0.025, 25 0.025})
    action (rand-nth [:add :minus])
    value (str ;(case action :add "+ " :minus "- ")
     effect)]
  (hash-map
  :id id
  :effect effect
  :action action
  :value value)))


(defn oxofn [x y]
  (into (sorted-map) (zipmap (map vec (combi/cartesian-product (range 0 x) (range 0 y)))
    (map mezo
      (range (* x y)))
  )))

(swap! app-state assoc :oxo (dissoc (oxofn 5 5) [(get @app-state :playx) (get @app-state :playy)]))


(defn action [ratom act]
  (let [
    playx (get @ratom :playx)
    playy (get @ratom :playy)
    k (case act
      :left [(dec playx) playy]
      :right [(inc playx) playy]
      :up [playx (dec playy)]
      :down [playx (inc playy)])
    v (get-in @ratom [:oxo k])
    [x y] k]
  (if v
    (do
      (swap! ratom assoc :playx x :playy y) ; frissít a lépés koordinátájára
      (swap! ratom update :hp
        #(case (get v :action)
          :add (+ % (get v :effect))
          :minus (- % (get v :effect))))
;            :dot (* % (get v :effect))
;            :divide (/ % (get v :effect))))
      (swap! ratom update :oxo #(dissoc % k))
      (loop [px playx, py playy]
        (let [
          empty-x px
          empty-y py
          px (case act
            :left (inc px)
            :right (dec px)
            px)
          py (case act
            :up (inc py)
            :down (dec py)
            py)]
        (if (get (get @ratom :oxo) [px py])
        (do
          (swap! ratom update :oxo #(dissoc (assoc % [empty-x empty-y]
            (get % [px py]))
            [px py]))
          (recur px py))
          (swap! ratom update :oxo #(dissoc (assoc % [empty-x empty-y]
            (mezo 0))
            [px py]))
          )))
  ))))

(.addEventListener
  js/document
  "keydown" (fn [event]
              (let [k (.-key event)]
              (cond
                (contains? #{"ArrowRight" \d} k) (action app-state :right)
                (contains? #{"ArrowLeft" \a} k) (action app-state :left)
                (contains? #{"ArrowUp" \w} k) (action app-state :up)
                (contains? #{"ArrowDown" \s} k) (action app-state :down)
:else (js/console.log (str "Unhandled key event: "k))))))


(defn gen-butt [ratom]
  (map (fn [[k v]]
      (let [[x y] k]
      [:button
      {:style {:width "77px" :height "77px" :position "absolute"
               :background-color (case (:effect v)
                 1 "#ddd"
                 2 "#ccc"
                 3 "#bbb"
                 4 "#aaa"
                 10 "#784"
                 25 "#321"
                 "#321")
               :left (* 77 x) :top (* 77 y)}
;               :transition "all 0.3s ease"}
      :value v
      :on-click (fn [] (action ratom k)
)}
       (get v :value)]))
    (get @ratom :oxo))
  )

(defn page [ratom]
  [:div {:style {:display "flex" :justify-content "center"}}
;  "Your table:"
    [:div {:style {:position "relative" :width "385px"}}
    (gen-butt ratom)
    [:div   {:style {:width "77px" :height "77px" :line-height "77px" :position "absolute"
               :left (* 77 (get @ratom :playx)) :top (* 77 (get @ratom :playy))
               :transition "all 0.3s ease" :display "flex" :justify-content "center"}}
              "Player: " (str (get @ratom :hp))]]
    [:button {
      :style {:width "77px" :height "77px" :position "absolute" :right 100 :top 40 :margin "50px"}
      :on-click (fn [] (dotimes [n 4] (hist/undo!)))} "Undo"]
    [:button {
      :style {:width "77px" :height "77px" :position "absolute" :right 100 :top 120 :margin "50px"}
      :on-click (fn [] (dotimes [n 4] (hist/redo!)))} "Redo"]
    [:div  {:style {:width "77px" :height "77px" :position "absolute" :right 100 :top 200 :margin "50px"}}
      (str "HP: " (get @ratom :hp))]
])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize App

(defn dev-setup []
  (when ^boolean js/goog.DEBUG
    (enable-console-print!)
    (println "dev mode")
    ))

(defn reload []
  (reagent/render [page app-state]
                  (.getElementById js/document "app")))

(defn ^:export main []
  (dev-setup)
  (reload)
;  (doseq [x (range 1000)] (action app-state (rand-nth [:right :left :up :down])))
  )
