(ns dirty-vegetables.core
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as string]))


(def units {"cup" {:unit/type :unit.type/volume
                   :unit/name "cup"
                   :unit/factor 236.588}
            "tablespoon" {:unit/type :unit.type/volume
                          :unit/name "tablespoon"
                          :unit/factor 14.7868}
            "teaspoon" {:unit/type :unit.type/volume
                        :unit/name "teaspoon"
                        :unit/factor 4.92892}
            "oz" {:unit/type :unit.type/mass
                  :unit/name "oz"
                  :unit/factor 28.3495}
            "pound" {:unit/type :unit.type/mass
                     :unit/name "pound"
                     :unit/factor 453.592}
            "gram" {:unit/type :unit.type/mass
                    :unit/name "gram"
                    :unit/factor 1}
            "mL" {:unit/type :unit.type/volume
                  :unit/name "mL"
                  :unit/factor 1}
            "count" {:unit/type :unit.type/quantity
                     :unit/name "count"
                     :unit/factor 1}})

(def all-units-sorted
  (sort-by :unit/name (vals units)))

(def default-how-much-unit-name "gram")

(defn units-of-type
  [unit-type]
  (filter #(= unit-type (:unit/type %)) (vals units)))


(def integer-pattern #"\d+")
(def fraction-pattern #"(\d+ )?\d+/\d+")
(def decimal-pattern #"(\d+)?\.\d+")

(defn matches?
  [pattern]
  (fn [s]
    (some? (re-matches pattern s))))

(spec/def :input/number (spec/and string?
                                  (spec/or :fraction (matches? fraction-pattern)
                                           :integer (matches? integer-pattern)
                                           :decimal (matches? decimal-pattern))))

(spec/def :input/name (spec/and string? not-empty))

(spec/def :input/measurement
  (spec/tuple :input/number :unit/name))

(spec/def :unit/unit
  (spec/keys :req [:unit/type :unit/name :unit/factor]))


(spec/def :unit/name #(contains? units %))
(spec/def :unit/factor pos?)
(spec/def :unit/type #{:unit.type/volume :unit.type/mass :unit.type/quantity})

(spec/def :ingredient/input
  (spec/keys :req [:ingredient/name :ingredient/calorie-density]))

(spec/def :ingredient/calorie-density
  (spec/and
    (spec/map-of :unit/type (spec/keys :req [:calorie-density/measurement
                                             :calorie-density/calories]))
    #(< 0 (count %))))

(spec/def :calorie-density/measurement :input/measurement)
(spec/def :calorie-density/calories integer?)

(spec/def :db/id (spec/and string? not-empty))
(spec/def :ingredient/id :db/id)
(spec/def :ingredient/name :input/name)


(spec/def :recipe/input
  (spec/keys :req [:recipe/name
                   :recipe/ingredients
                   :recipe/totals]
             :opt [:recipe/notes]))


(spec/def :recipe/name :input/name)
(spec/def :recipe/notes string?)

(spec/def :recipe/ingredients
  (spec/and (spec/coll-of :recipe/ingredient)
            #(< 0 (count %))))

(spec/def :recipe/ingredient
  (spec/keys :req [:input/measurement
                   :ingredient/id]))


(spec/def :recipe/totals
  (spec/and
    (spec/map-of :unit/type :input/measurement)
    #(< 0 (count %))))


(defn read-fraction
  [s]
  (let [[x y] (string/split s #" ")]
    (if (some? y)
      (+ (js/parseInt x) (read-fraction y))
      (let [[x y] (string/split s #"/")]
        (/ (js/parseInt x) (js/parseInt y))))))


(defn read-input-number
  [s]
  (if-let [[kind v] (spec/conform :input/number (string/trim s))]
    (cond (= kind :fraction) (read-fraction v)
          (= kind :decimal) (js/parseFloat v)
          (= kind :integer) (js/parseInt v 10))))


(defn sub-ingredients
  [recipe ingredients]
  (update recipe
          :recipe/ingredients
          (fn [is]
            (map (fn [{:keys [ingredient/id] :as ri}]
                   (assoc ri
                          :ingredient
                          (get ingredients id)))
                 is))))


(defn calc-ingredient-density
  [ingredient unit-type]
  (let [density (get-in ingredient [:ingredient/calorie-density unit-type])
        [m u] (:calorie-density/measurement density)
        cal (:calorie-density/calories density)
        measure (read-input-number m)
        {factor :unit/factor} (get units u)]
    (/ cal (* measure factor))))


(defn calories-in
  "Calculates the total calories for the recipe"
  [{:keys [recipe/ingredients] :as recipe}]
  (reduce (fn [total {[m u] :input/measurement in :ingredient}]
            (let [amount (read-input-number m)
                  unit (get units u)
                  density (calc-ingredient-density in (:unit/type unit))
                  base-amount (* amount (:unit/factor unit))]
              (+ total (* density base-amount))))
          0
          ingredients))


(defn index-ingredients
  [ingredients]
  (reduce (fn [idx ingredient]
            (assoc idx (:db/id ingredient) ingredient))
          {}
          ingredients))


(defn how-much
  "Calculate how many calories are in a given amount of a recipe"
  [recipe ingredients amount unit-name]
  (let [ingredient-idx (index-ingredients ingredients)
        total-calories (calories-in (sub-ingredients recipe ingredient-idx))
        amount-unit (get units unit-name)
        [total-amount total-unit] (get-in recipe [:recipe/totals (:unit/type amount-unit)])
        total-amount-in-base (* (read-input-number total-amount)
                                (get-in units [total-unit :unit/factor]))
        amount-in-base (* (read-input-number amount)
                          (:unit/factor amount-unit))]
    (* amount-in-base (/ total-calories total-amount-in-base))))


(defn has-calorie-density-for-unit?
  [ingredient unit-name]
  (let [types (-> ingredient
                  :ingredient/calorie-density
                  keys
                  set)
        unit (get-in units [unit-name :unit/type])]
    (contains? types unit)))


(def default-mass-unit "gram")
(def default-volume-unit "cup")
(def default-quantity-unit "count")


(def default-densities
  {:unit.type/mass {:calorie-density/measurement [nil default-mass-unit]}
   :unit.type/volume {:calorie-density/measurement [nil default-volume-unit]}
   :unit.type/quantity {:calorie-density/measurement [nil default-quantity-unit]}})


(def new-ingredient
  {:db/id "new"
   :ingredient/name "New Ingredient"
   :ingredient/calorie-density default-densities})

(defn new-recipe-ingredient
  [ingredient-id]
  {:input/measurement [nil "cup"]
   :ingredient/id ingredient-id})


(def new-recipe
  {:db/id "new"
   :recipe/name "New Recipe"
   :recipe/notes ""
   :recipe/ingredients []
   :recipe/totals {:unit.type/mass [nil default-mass-unit]
                   :unit.type/volume [nil default-volume-unit]
                   :unit.type/quantity [nil default-quantity-unit]}})
