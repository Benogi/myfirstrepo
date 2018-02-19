(ns rpg-web.core
  (:require
   [reagent.core :as reagent]
   [historian.core :as hist]
   [clojure.math.combinatorics :as combi]
   [cljs.reader :as read]
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

(defonce app-state
  (reagent/atom {}))

(hist/record! app-state :app-state)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page




(defn selectcount [app-state]
  (count (keep identity (map (fn [[k v]] (if (get v :select?) true nil))
    @app-state)))
  )

(defn selected [app-state]
  [:div (selectcount app-state)]
  )

(defn selectedid [app-state]
  (first (keep (fn [[k v]] (when (get v :select?) k))
    @app-state))
  )

(defn move [id app-state]
  (let
    [stand (selectedid app-state)
     babu (get-in @app-state [stand :value])]
    (swap! app-state assoc-in [stand :value] nil)
    (swap! app-state assoc-in [stand :select?] nil)
    (swap! app-state assoc-in [id :value] babu))
  )

(defn select! [id app-state]
  (swap! app-state assoc-in [id :select?]
    (if (get-in @app-state [id :select?]) false true))
  )

(defn buttoner [id app-state]
  [:input {:type "button"
    :class "butt"
    :value (if (get-in @app-state [id :value]) (get-in @app-state [id :value]) id)    ;nil alapvalue esetén !!!!!!!!!!!!!!
    :style (merge
              (if (get-in @app-state [id :select?]) {:background-color "blue" :color "yellow"} {})
              {:width "77px" :height "77px"})
    :on-click (fn [] (if (= (selectcount app-state) 1) (move id app-state) (select! id app-state)))
    }])

(defn mapmaker [app-state x]
       (map
          (fn [l]
            [:div
              (map
                (fn [n]
                  (swap! app-state assoc (str l n)
                    {:value
                      (cond
;  not working                      (let [l (vec (take 1 (rand-nth (vec "ABCDEFGHIJKLMNOPQRSTUVZ"))))
;                              n (vec (take 1 (rand-nth (range 1 x))))]
;                             (str l n))
;                        (str "You: " (rand-nth (range 1 10)))
;                       :else (fn []
;                               (rand-nth ["+ " "- " "/ " "* "])
;                                 (cond
;                                   (or "+ " "- ") (rand-nth (range 1 10))
;                                   (or "/ " "* ") 2)))
                        :else (str (rand-nth ["+ " "- " "/ " "* "]) (rand-nth (range 1 10))))
                     :select? false})
                  [buttoner (str l n) app-state])
                (vec (range 1 x)))])
          (vec (take (- x 1) "ABCDEFGHIJKLMNOPQRSTUVZ"))))

(defn buttonjack [x]
  [:input {:type "button"
    :class "butt"
    :value (str x)
    :style {:width "70px" :height "70px"}
    :on-click (fn [] ())
    }]
  )

(defn lumberjack [x]
  (map buttonjack (map (fn [] (rand-nth (combi/cartesian-product '(+ - / *) (range 1 6)))) (range x))))

;nem ez a cél, viszont igaz a teljes szekvenciára {[x y] ab, ...},
;hogy x + a = y + b , ahol az abc-ben hátrébb álló betű a kisebb helyiértékű számjegyhez tartozik.
;(defn oxo [x y]
;  (str (zipmap (sort > (map vec (combo/cartesian-product (range 0 x) (range 0 y)))) (range 100))))

(defn map-map
    "Returns a new map with each key-value pair in `m` transformed by `f`.
    `f` takes the arguments `[key value]` and should return a value castable to a map entry,
    such as `{transformed-key transformed-value}`."
    [f m]
    (into (empty m) (map #(apply f %) m)) )

(defn map-map-keys [f m]
    (map-map (fn [key value] {(f key) value}) m) )

(defn map-map-values [f m]
    (map-map (fn [key value] {key (f value)}) m) )


(defn arities [v]
  (->> v
    meta
    :arglists
    (map #(remove #{'&} %))
    (map count)))

(defn power [x k]
  (if (or (nil? k) (= 0 k)) 1
    (loop [result x, remaining k]
      (if (= remaining 1)
        result
        (recur (* result x) (dec remaining))))))

;(defn sum-by-place-value [k m x & a]
;  "Sum (from k=0 to k=n-1) (* (power x k) (a idx=n-k))"
;  (dotimes [n (count (take (- m 1) (iterate inc k)))] (* a.m-k (power x k)))
;  )


(defn oxofn [x y]
  (into (sorted-map) (zipmap (map vec (combi/cartesian-product (range 0 x) (range 0 y))) (range (* x y)))))
(defn get-oxo [x y]
  (get {[x y] ((read/read-string (str x y)))} [x y]))



;not working
;(defn gen-butt [x y]
;  (map-map (fn [] (
;    [:button {
;      :style {:width "77px" :height "77px"}
;      :value nil
;      :on-click (fn [] ())}]))
;    (oxo x y))
;  )

(defn page [ratom]
  [:div
    [:p {:style {:position "absolute" :left 315 :margin "50px" :margin-top "10px" :width "500px"}}
    ;[selected ratom]
    "Your table:"
    [:br][:br][:br][:br]
;not working    (get-oxo 13 1111)
;not working    (gen-butt 10 10)]
;bullshit    (get {[0 2] 2} [0 2])
; why not working?    (map? (oxofn 10 10)]
    (oxofn 10 10)] ; if x y >10 !!!!!!
;works   (lumberjack 25)]
;works   (mapmaker ratom 6)]
    [:button {
      :style {:width "77px" :height "77px" :position "absolute" :right 100 :top 40 :margin "50px"}
      :on-click (fn [] (dotimes [n 4] (hist/undo!)))} "Undo"]
    [:button {
      :style {:width "77px" :height "77px" :position "absolute" :right 100 :top 120 :margin "50px"}
      :on-click (fn [] (dotimes [n 4] (hist/redo!)))} "Redo"]

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
  (reload))
