(ns dirty-vegetables.app
  (:require [bide.core :as route]
            [dirty-vegetables.fauna :as fauna]
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
  (swap! router-state assoc :route [name params query]))


(def views
  {:app/home views/home
   :app/ingredient-list views/ingredient-list
   :app/ingredient-detail views/ingredient-detail
   :app/recipe-list views/recipe-list
   :app/recipe-detail views/recipe-detail})

;    <header>
;      <h1>Dirty Vegetables</h1>
;      <div class="sub-header">
;        <ul class="nav">
;          <li>
;            <a href="#/home">How Many?</a>
;          </li>
;          <li>
;            <a href="#/ingredients">Ingredients</a>
;          </li>
;          <li>
;            <a href="#/recipes">Recipe</a>
;          </li>
;        </ul>
;        <div class="auth" data-netlify-identity-menu></div>
;      </div>
;    </header>

(defn  on-reset-key-click
  [e]
  (.preventDefault e)
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
  #_(id/start!)
  (fauna/init! handle-auth-event)
  (route/start! router {:default :app/home 
                        :on-navigate on-navigate})
  (rd/render [scaffold] (js/document.getElementById "app")))
