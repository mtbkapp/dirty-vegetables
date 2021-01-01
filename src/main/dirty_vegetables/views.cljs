(ns dirty-vegetables.views
  (:require [clojure.spec.alpha :as spec]
            [dirty-vegetables.core :as core]
            [reagent.core :as r]
            [reagent.dom :as rd]))


(defn home
  [[params query]]
  [:div "home"])


(defn ingredient-list
  [[params query]]
  [:div {:id "ingredient-list"}
   [:div [:h2 "Ingredients"]
    [:a {:href "#/ingredients/new"} "New Ingredient"]
    ]])


(defn on-cancel-click
  []
  (if (js/confirm "Are you sure?")
    (js/history.back)))


(defn empty-val?
  [x]
  (or (nil? x) (= "" x)))


(defn parse-int
  [x]
  (let [p (js/parseInt x 10)]
    (if (not (js/isNaN p))
      p)))


(defn name-input
  [id label value on-change]
  [:div
   [:label {:for "ingredient-name"} label]
   [:input {:id "ingredient-name"
            :type "text"
            :value value 
            :on-change #(on-change (.-value (.-target %)))}]])


(def unit-type->label
  {:unit.type/mass "Mass: "
   :unit.type/volume "Volume: "
   :unit.type/quantity "Quantity: "})


(defn calorie-density-input
  [unit-type density on-change]
  (let [density-for-unit (get density unit-type)
        [number unit-name] (:calorie-density/measurement density-for-unit)
        calories (:calorie-density/calories density-for-unit)
        trigger-change (fn [n u c]
                         (on-change 
                           unit-type
                           {:calorie-density/measurement [n u]
                            :calorie-density/calories c}))
        unit-options (map (fn [unit]
                            [:option {:value (:unit/name unit)} (:unit/name unit)])
                          (core/units-of-type unit-type))]
    [:div
     [:label (unit-type->label unit-type)]
     [:input {:type "text"
              :value (str number)
              :on-change (fn [e]
                           (trigger-change
                             (.-value (.-target e)) 
                             unit-name
                             calories))}]
     (into [:select {:value (str unit-name)
                     :on-change (fn [e]
                                  (trigger-change
                                    number
                                    (.-value (.-target e))
                                    calories))}]
           unit-options)
     [:input {:type "number"
              :value (str calories)
              :on-change (fn [e]
                           (trigger-change
                             number
                             unit-name
                             (.-value (.-target e))))}]]))

(defn error-list
  [errors]
  (when (not-empty errors)
    [:div {:class "error"} "Errors:"
     (into [:ul]
           (map (fn [e]
                  [:li e])
                errors))]))


(defn clean-densities
  [ds]
  (reduce (fn [new-ds [unit-type {c :calorie-density/calories
                                  [n unit-name] :calorie-density/measurement}]]
            (if (and (empty-val? c)
                     (empty-val? n))
              (dissoc new-ds unit-type)
              (update-in new-ds [unit-type :calorie-density/calories] parse-int)))
          ds
          ds))


(defn validate-ingredient
  [ingredient]
  (->> (update ingredient :ingredient/calorie-density clean-densities)
       (spec/explain-data :ingredient/input)
       (:cljs.spec.alpha/problems)
       (map (fn [{:keys [path] :as problem}]
              (let [[p0 p1 p2] path]
                (cond (= [:ingredient/calorie-density] path)
                      "At least one calorie density must be entered."
                      (and (= :ingredient/calorie-density p0)
                           (= :calorie-density/calories p2))
                      "Calories must be a whole number."
                      (and (= :ingredient/calorie-density p0)
                           (= :calorie-density/measurement p2))
                      "Measurement must a whole number, a decimal, or a fraction."
                      (= [:ingredient/name] path)
                      "Ingredient name must not be empty."))))
       (distinct)))


(defn on-ingredient-save-click
  [error-ref ingredient]
  (let [es (validate-ingredient ingredient)]
    (if (empty? es)
      (reset! error-ref [])
      (reset! error-ref es))))


(defn ingredient-detail*
  [ingredient]
  (let [state (r/atom ingredient)
        update-density! (fn [unit-type new-density]
                          (swap! state
                                 assoc-in 
                                 [:ingredient/calorie-density unit-type]
                                 new-density))
        errors (r/atom [])]
    (fn []
      [:div
       [name-input 
        "ingredient-name"
        "Ingredient Name: "
        (:ingredient/name @state)
        (fn [new-name]
          (swap! state assoc :ingredient/name new-name))]
       [:h4 "Calorie Density"]
       [calorie-density-input
        :unit.type/mass
        (:ingredient/calorie-density @state)
        update-density!]
       [calorie-density-input
        :unit.type/volume
        (:ingredient/calorie-density @state)
        update-density!]
       [calorie-density-input
        :unit.type/quantity
        (:ingredient/calorie-density @state)
        update-density!]
       [:div
        [:button {:type "button" :on-click on-cancel-click} "Cancel"]
        [:button {:type "button"
                  :on-click #(on-ingredient-save-click errors @state)}
         "Save"]]
       [error-list @errors]])))


(def default-mass-unit "gram")
(def default-volume-unit "cup")
(def default-quantity-unit "count")


(def new-ingredient
  {:ingredient/name "New Ingredient" 
   :ingredient/calorie-density {:unit.type/mass {:calorie-density/measurement [nil default-mass-unit]}
                                :unit.type/volume {:calorie-density/measurement [nil default-volume-unit]}
                                :unit.type/quantity {:calorie-density/measurement [nil default-quantity-unit]}}})


(defn ingredient-detail
  [[{:keys [id]} query]]
  (if (= "new" id)
    [ingredient-detail* new-ingredient]))


(defn recipe-list
  [[params query]]
  [:div "recipe-list"])


(defn recipe-detail
  [[params query]]
  [:div (str "recipe-detail" params)])

