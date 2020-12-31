(ns dirty-vegetables.views
  (:require [dirty-vegetables.core :as core]
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


(defn on-ingredient-save-click
  [ingredient]
  (prn "save" ingredient))


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
        [number unit-name] (:calorie-density/measure density-for-unit)
        calories (:calorie-density/calories density-for-unit)
        trigger-change (fn [n u c]
                         (on-change 
                           unit-type
                           {:calorie-density/measure [n u]
                            :calorie-density/calories c}))]
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
           (map (fn [unit]
                  [:option {:value (:unit/name unit)} (:unit/name unit)])
                (core/units-of-type unit-type)))
     [:input {:type "number"
              :value (str calories)
              :on-change (fn [e]
                           (trigger-change
                             number
                             unit-name
                             (.-value (.-target e))))}]]))


(defn ingredient-detail*
  [ingredient]
  (let [state (r/atom ingredient)
        update-density! (fn [unit-type new-density]
                          (swap! state
                                 assoc-in 
                                 [:ingredient/calorie-density unit-type]
                                 new-density))]
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
        [:button {:type "button" :on-click #(on-ingredient-save-click @state)} "Save"]]])))


(def new-ingredient
  {:ingredient/name "New Ingredient" 
   :ingredient/calorie-density {}})


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

