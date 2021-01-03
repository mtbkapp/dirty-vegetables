(ns dirty-vegetables.views
  (:require [clojure.spec.alpha :as spec]
            [dirty-vegetables.core :as core]
            [dirty-vegetables.fauna :as fauna]
            [reagent.core :as r]
            [reagent.dom :as rd]))


(defn home
  [[params query]]
  [:div "home"])


(def fauna-err->msg
  {::fauna/no-key "Db key not set. How'd you get here?"
   ::fauna/not-implemented "Not implemented."
   ::fauna/save-error "Error saving to database."
   ::fauna/read-error "Error reading from the database."})


(defn target-val
  [e]
  (.-value (.-target e)))


(defn ingredient-list
  [_]
  (let [state (r/atom {:error nil :fetching? true})]
    (-> (fauna/fetch-all-ingredients)
        (.then (fn [data]
                 (swap! state assoc :fetching? false :data data)))
        (.catch (fn [err]
                  (swap! state
                         assoc
                         :error (fauna-err->msg err)
                         :fetching? false))))
    (fn []
      (let [{:keys [error fetching? data]} @state]
        [:div {:id "ingredient-list"}
         [:div [:h2 "Ingredients"]
          [:a {:href "#/ingredients/new"} "New Ingredient"]
          (cond fetching? [:div "Loading..."]
                (some? error) [:span {:class "error"} error]
                :else (into [:ul]
                            (map (fn [in]
                                   [:li 
                                    [:a 
                                     {:href (str "#/ingredients/" (:db/id in))}
                                     (:ingredient/name in)]]))
                            data))]]))))


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
            :on-change #(on-change (target-val %))}]])


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
                           (trigger-change (target-val e)
                                           unit-name
                                           calories))}]
     (into [:select {:value (str unit-name)
                     :on-change (fn [e]
                                  (trigger-change number
                                                  (target-val e)
                                                  calories))}]
           unit-options)
     [:input {:type "number"
              :value (str calories)
              :on-change (fn [e]
                           (trigger-change number
                                           unit-name
                                           (target-val e)))}]]))

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
  (->> ingredient 
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
  (let [cleaned (update ingredient :ingredient/calorie-density clean-densities)
        es (validate-ingredient cleaned)]
    (if (empty? es)
      (do 
        (reset! error-ref [])
        (-> (fauna/save-ingredient cleaned)
            (.then (fn []
                     (js/history.back)))
            (.catch (fn [err]
                      (reset! error-ref [(fauna-err->msg err)])))))
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
        :unit.type/volume
        (:ingredient/calorie-density @state)
        update-density!]
       [calorie-density-input
        :unit.type/mass
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


(defn ingredient-detail
  [[{:keys [id]} query]]
  (let [loading (r/atom nil)]
    (-> (fauna/read-single-ingredient id)
        (.then #(reset! loading %))
        (.catch #(reset! loading :error)))
    (fn []
      (let [x @loading]
        (cond (nil? x) [:div "Loading..."]
              (= :error x) [:div "Error loading ingredient"]
              :else (ingredient-detail* x))))))


(defn recipe-list
  [_]
  (let [state (r/atom {:error nil :fetching? true})]
    (-> (fauna/fetch-all-recipes)
        (.then (fn [data]
                 (swap! state assoc :fetching? false :data data)))
        (.catch (fn [err]
                  (swap! state
                         assoc
                         :error (fauna-err->msg err)
                         :fetching? false))))
    (fn []
      (let [{:keys [error fetching? data]} @state]
        [:div {:id "ingredient-list"}
         [:div [:h2 "recipe"]
          [:a {:href "#/recipes/new"} "New Recipe"]
          (cond fetching? [:div "Loading..."]
                (some? error) [:span {:class "error"} error]
                :else (into [:ul]
                            (map (fn [in]
                                   [:li 
                                    [:a 
                                     {:href (str "#/recipes/" (:db/id in))}
                                     (:ingredient/name in)]]))
                            data))]]))))


(defn recipe-detail
  [[{:keys [id]} query]]
  (let [loading (r/atom nil)]
    (-> (fauna/read-single-recipe id)
        (.then #(reset! loading %))
        (.catch #(reset! loading :error)))))

