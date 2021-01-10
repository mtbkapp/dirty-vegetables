(ns dirty-vegetables.views
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [dirty-vegetables.core :as core]
            [dirty-vegetables.fauna :as fauna]
            [reagent.core :as r]
            [reagent.dom :as rd]))


(def fauna-err->msg
  {::fauna/no-key "Db key not set. How'd you get here?"
   ::fauna/not-implemented "Not implemented."
   ::fauna/save-error "Error saving to database."
   ::fauna/read-error "Error reading from the database."})


(defn target-val
  [e]
  (.-value (.-target e)))


(defn unit-option
  [unit]
  [:option {:value (:unit/name unit)} (:unit/name unit)])


(defn unit-select
  [{:keys [selected units on-change]}]
  (into [:select {:value selected
                  :on-change #(on-change (target-val %))}]
        (map unit-option)
        units))


(defn on-how-much-click
  [state]
  ; check if input is valid
  ; if not, add errors to state
  ; if valid put the answer calorie-count  
  )


(defn combine-foods
  [recipes ingredients]
  []
  )

(defn food-select
  [{:keys [selected foods on-change]}]
  [:select]
  )


(defn home
  [[params query]]
  (let [state (r/atom {:amount ""
                       :unit core/default-how-much-unit-name 
                       :selected-food nil
                       :errors []
                       :calorie-count nil
                       :fetching? true})]
    (-> (fauna/fetch-all-recipes-and-ingredients)
        (.then (fn [{:keys [recipes ingredients]}]
                 (let [foods (combine-foods recipes ingredients)]
                 (swap! state assoc
                        :fetching? false
                        :foods foods
                        :selected-food (first foods)))))
        (.catch (fn [err]
                  (swap! state assoc
                         :fetching? false
                         :errors ["Error loading data"]))))
    (fn []
      (if (:fetching? @state)
        [:div "Loading..."]
        [:div
         [:h4 "How many calories are in:"]
         [:input {:type "text"
                  :value (:amount @state) 
                  :on-change #(swap! state assoc :amount (target-val %))}]
         [unit-select {:selected (:unit @state)
                       :units core/all-units-sorted
                       :on-change #(swap! state assoc :unit %)}]
         [food-select {:selected (:selected-food @state)
                       :foods (:foods @state)
                       :on-change #(swap! state assoc :selected-food %)}]
         [:button {:type "button"
                   :on-click #(swap! state on-how-much-click)}
          "Go"]]))))


(defn sort-by-lcase
  [key-fn coll]
  (sort-by (comp string/lower-case key-fn) coll))


(defn ingredient-list
  [_]
  (let [state (r/atom {:error nil :fetching? true})]
    (-> (fauna/fetch-all-ingredients)
        (.then (fn [data]
                 (swap! state
                        assoc
                        :fetching? false
                        :data (sort-by-lcase :ingredient/name data))))
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
  [changed?]
  (prn changed?)
  (if changed?
    (if (js/confirm "Are you sure?")
      (js/history.back))
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
       (remove nil?)
       (distinct)))


(defn on-ingredient-save-click
  [error-ref ingredient]
  (let [cleaned (update ingredient :ingredient/calorie-density clean-densities)
        es (validate-ingredient cleaned)]
    (if (empty? es)
      (do
        (reset! error-ref [])
        (-> (fauna/save-rec "ingredients" cleaned)
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
        [:button 
         {:type "button"
          :on-click #(on-cancel-click (not= ingredient @state))}
         "Cancel"]
        [:button {:type "button"
                  :on-click #(on-ingredient-save-click errors @state)}
         "Save"]]
       [error-list @errors]])))


(defn ingredient-detail
  [[{:keys [id]} query]]
  (let [loading (r/atom nil)]
    (-> (fauna/fetch-single-ingredient id)
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
                 (swap! state
                        assoc
                        :fetching? false
                        :data (sort-by-lcase :recipe/name data))))
        (.catch (fn [err]
                  (swap! state
                         assoc
                         :error (fauna-err->msg err)
                         :fetching? false))))
    (fn []
      (let [{:keys [error fetching? data]} @state]
        [:div {:id "recipe-list"}
         [:div [:h2 "Recipes"]
          [:a {:href "#/recipes/new"} "New Recipe"]
          (cond fetching? [:div "Loading..."]
                (some? error) [:span {:class "error"} error]
                :else (into [:ul]
                            (map (fn [in]
                                   [:li
                                    [:a
                                     {:href (str "#/recipes/" (:db/id in))}
                                     (:recipe/name in)]]))
                            data))]]))))


(defn runit-select
  [unit-type {:keys [unit-name on-change]}]
  (let [unit-options (map (fn [unit]
                            [:option {:value (:unit/name unit)} (:unit/name unit)])
                          (core/units-of-type unit-type))]
    (into [:select {:value (str unit-name)
                    :on-change #(on-change (target-val %))}]
          unit-options)))


(defn recipe-total-input
  [totals unit-type on-change]
  (let [[amount unit-name] (get totals unit-type)]
    [:div
     [:label (unit-type->label unit-type)]
     [:input {:value (str amount)
              :on-change #(on-change [unit-type [(target-val %) unit-name]])}]
     [runit-select unit-type {:unit-name unit-name
                             :on-change #(on-change [unit-type [amount %]])}]]))


(defn recipe-totals
  [totals on-change]
  [:div [:h4 "Totals:"]
   [recipe-total-input totals :unit.type/mass on-change]
   [recipe-total-input totals :unit.type/volume on-change]
   [recipe-total-input totals :unit.type/quantity on-change]])


(defn ingredient-select
  [{:keys [selected-id all-ingredients on-change]}]
  (into [:select {:value (str selected-id)
                  :on-change #(on-change (target-val %))}]
        (map (fn [{:keys [db/id ingredient/name]}]
               [:option {:value id} name]))
        all-ingredients))


(defn all-unit-select
  [{:keys [value on-change]}]
  (into [:select {:value (str value)
                  :on-change #(on-change (target-val %))}]
        (map (fn [{n :unit/name}]
               [:option {:value n} n]))
        core/all-units-sorted))


(defn recipe-ingredient-line
  [idx in all-ingredients on-change]
  [:li
   [:input {:type "name"
            :value (-> in :input/measurement first)
            :on-change #(on-change [:update idx (assoc-in in
                                                          [:input/measurement 0]
                                                          (target-val %))])}]
   [all-unit-select {:value (-> in :input/measurement second)
                     :on-change #(on-change [:update idx (assoc-in in
                                                                   [:input/measurement 1]
                                                                   %
                                                                   )])}]
   [ingredient-select {:selected-id (:ingredient/id in)
                       :all-ingredients all-ingredients
                       :on-change #(on-change [:update idx (assoc in :ingredient/id %)])}]
   [:button {:type "button" :on-click #(on-change [:delete idx])} "X"]])


(defn recipe-ingredient-list
  [recipe-ingredients all-ingredients on-change]
  [:div
   [:h4 "Ingredients:"]
   (into [:ul {:class "recipe-ingredients"}]
         (map-indexed (fn [idx in]
                        [recipe-ingredient-line idx in all-ingredients on-change])
              recipe-ingredients))
   [:button {:type "button" :on-click #(on-change [:add])} "Add ingredient to recipe"]])


(defn clean-totals
  [totals]
  (into {}
        (remove (fn [[unit-type [input unit-name]]]
                  (empty-val? input)))
        totals))


(defn validate-recipe
  [recipe]
  (->> recipe
       (spec/explain-data :recipe/input)
       (:cljs.spec.alpha/problems)
       (map (fn [{:keys [path]}]
              (let [[p0 p1] path]
                (cond (= [:recipe/name] path)
                      "Recipe name must not be empty."
                      (= [:recipe/totals] path)
                      "At least one total must be entered."
                      (= [:recipe/ingredients] path)
                      "At least one ingredient is required."
                      (= [:recipe/ingredients :input/measurement] [p0 p1])
                      "All ingredients need a measurement"))))
       (remove nil?)
       (distinct)))


(defn check-recipe-ingredient-types
  [{:keys [recipe/ingredients]} indexed-ingredients]
  (reduce (fn [acc {[_ unit-name] :input/measurement in-id :ingredient/id}]
            (let [{in-name :ingredient/name :as in} (get indexed-ingredients in-id)]
              (if (core/has-calorie-density-for-unit? in unit-name)
                acc
                (conj acc (str "Invalid unit for ingredient named " in-name)))))
          []
          ingredients))


(defn on-recipe-save-click
  [error-ref recipe indexed-ingredients]
  (let [cleaned (update recipe :recipe/totals clean-totals)
        es (into (validate-recipe cleaned)
                 (check-recipe-ingredient-types cleaned indexed-ingredients))]
    (if (empty? es)
      (do
        (reset! error-ref [])
        (-> (fauna/save-rec "recipes" cleaned)
            (.then (fn []
                     (js/history.back)))
            (.catch (fn [err]
                      (reset! error-ref [(fauna-err->msg err)])))))
      (reset! error-ref es))))


(defn recipe-detail*
  [{:keys [recipe ingredients]}]
  (let [errors (r/atom [])
        sorted-ingredients (sort-by-lcase :ingredient/name ingredients)
        indexed-ingredients (reduce #(assoc %1 (:db/id %2) %2) {} ingredients)
        state (r/atom recipe)
        new-ingredient (-> sorted-ingredients first :db/id core/new-recipe-ingredient)]
    (fn []
      [:div
       [name-input
        "recipe-name"
        "Recipe Name: "
        (:recipe/name @state)
        #(swap! state assoc :recipe/name %)]
       [recipe-totals
        (:recipe/totals @state)
        (fn [[unit-type new-measure]]
          (swap! state assoc-in [:recipe/totals unit-type] new-measure))]
       [recipe-ingredient-list
        (:recipe/ingredients @state)
        sorted-ingredients
        (fn [[method idx new-data]]
          (swap! state update :recipe/ingredients
                 (fn [is]
                   (cond (= :add method) (conj is new-ingredient)
                         (= :delete method) (into (subvec is 0 idx) (subvec is (inc idx)))
                         (= :update method) (assoc is idx new-data)
                         :else is))))]
       [:div
        [:label {:for "recipe-notes"} [:h4 "Notes:"]]
        [:textarea
         {:id "recipe-notes"
          :on-change #(swap! state assoc :recipe/notes (target-val %))
          :value (:recipe/notes @state)
          :rows 10
          :cols 50}]]
       [:button
        {:type "button"
         :on-click #(on-cancel-click (not= recipe @state))}
        "Cancel"]
       [:button
        {:type "button"
         :on-click #(on-recipe-save-click errors @state indexed-ingredients)}
        "Save"]
       [error-list @errors]])))


(defn recipe-detail
  [[{:keys [id]} query]]
  (let [loading (r/atom nil)]
    (-> (fauna/fetch-recipe-and-all-ingredients id)
        (.then #(reset! loading %))
        (.catch #(reset! loading :error)))
    (fn []
      (let [x @loading]
        (cond (nil? x) [:div "Loading..."]
              (= :error x) [:div "Error loading recipe"]
              :else [recipe-detail* x])))))

