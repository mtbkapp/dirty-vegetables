(ns dirty-vegetables.app
  (:require [bide.core :as route]
            [dirty-vegetables.views :as views]
            [reagent.core :as r]
            [reagent.dom :as rd]))


(def router
  (route/router [["/" :app/home]
                 ["/ingredients" :app/ingredient-list]
                 ["/ingredients/:id" :app/ingredient-detail]
                 ["/recipes" :app/recipe-list]
                 ["/recipes/:id" :app/recipe-detail]]))


(def router-state
  (r/atom nil))


(defn on-navigate
  "A function which will be called on each route change."
  [name params query]
  (reset! router-state [name params query]))


(def views
  {:app/home views/home
   :app/ingredient-list views/ingredient-list
   :app/ingredient-detail views/ingredient-detail
   :app/recipe-list views/recipe-list
   :app/recipe-detail views/recipe-detail})


(defn scaffold
  []
  (let [[route & args] @router-state]
    ;TODO put auth here
    [(get views route) args]))


(defn init
  []
  (route/start! router {:default :app/home 
                        :on-navigate on-navigate})
  (rd/render [scaffold] (js/document.getElementById "app")))
