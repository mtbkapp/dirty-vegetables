(ns dirty-vegetables.views
  (:require [reagent.core :as r]
            [reagent.dom :as rd]))


(defn home
  [[params query]]
  [:div "home"])


(defn ingredient-list
  [[params query]]
  [:div "ingredient list"])


(defn ingredient-detail
  [[params query]]
  [:div (str "ingredient-detail" params)])


(defn recipe-list
  [[params query]]
  [:div "recipe-list"])


(defn recipe-detail
  [[params query]]
  [:div (str "recipe-detail" params)])

