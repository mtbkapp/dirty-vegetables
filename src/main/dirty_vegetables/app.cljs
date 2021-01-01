(ns dirty-vegetables.app
  (:require [bide.core :as route]
            [dirty-vegetables.fauna :as fauna]
            [dirty-vegetables.views :as views]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!
                               oget+ oset!+ ocall+ oapply+ ocall!+ oapply!+]]
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
  (swap! router-state assoc :route [name params query]))


(def views
  {:app/home views/home
   :app/ingredient-list views/ingredient-list
   :app/ingredient-detail views/ingredient-detail
   :app/recipe-list views/recipe-list
   :app/recipe-detail views/recipe-detail})


(defn  on-reset-key-click
  [e]
  (ocall e "preventDefault")
  (fauna/reset-manaul-key))


(defn header
  []
  [:header
   [:h1 "Dirty Vegetables"]
   [:div {:class "sub-header"}
    [:ul {:class "nav"}
     [:li [:a {:href "#/home"} "How Many?"]]
     [:li [:a {:href "#/ingredients"} "Ingredients"]]
     [:li [:a {:href "#/recipes"} "Recipe"]]
     [:li [:a {:href "#/" :on-click on-reset-key-click} "db"]]]
    [:div {:class "auth" :data-netlify-identity-menu true}]]])


(defn scaffold
  []
  (let [{auth? :auth? [route & args] :route} @router-state]
    [:div
     [header]
     (if auth?
       [(get views route) args]
       "Please login or enter a db key") ]))


(defn handle-auth-event
  [ev]
  (swap! router-state assoc :auth? (= ev :login)))


(defn init
  []
  (fauna/init! handle-auth-event)
  (route/start! router {:default :app/home 
                        :on-navigate on-navigate})
  (rd/render [scaffold] (js/document.getElementById "app")))
