(ns dirty-vegetables.identity
  (:require [reagent.core :as r]))


(def state (r/atom {:ready? false}))


(defn fetch-fauna-key*
  [jwt]
  (-> (js/fetch
        "/.netlify/functions/faunakey"
        #js {:headers #js {"Content-Type" "application/json"
                           "Authorization" (str "Bearer " jwt)}})
      (.then (fn [resp]
               (if (.-ok resp)
                 (-> (.json resp)
                     (.then (fn [body]
                              (js/console.log "fk" body)
                              (swap! state 
                                     assoc
                                     :ready? true
                                     :fauna-key (.-faunaKey ^object body))))))
               (do
                 (js/console.error "non 200 trying to get fauna key")
                 (swap! state assoc :error true))))))


(defn fetch-fauna-key
  []
  (if-let [user (js/netlifyIdentity.currentUser)]
    (-> (.jwt user)
        (.then fetch-fauna-key*)
        (.catch (fn [err]
                  (js/console.error "error getting netlify jwt" err))))
    (js/console.error "error getting fauna key: not logged in")))


(if-let [user (js/netlifyIdentity.currentUser)]
    (-> (.jwt user)
        (.then (fn [jwt]
                 (js/fetch
                   ".netlify/functions/faunakey"
                   #js {:headers #js {"Content-Type" "application/json"
                                      "Authorization" (str "Bearer " jwt)}})))
        (.catch (fn [err]
                  (js/console.error err))))
    
    (js/console.log "not logged in"))


(defn start!
  []
  (js/console.log "starting id events")
  (js/netlifyIdentity.on 
    "login"
    (fn [user]
      (swap! state assoc :user user)
      (fetch-fauna-key)))
  (js/netlifyIdentity.on
    "logout"
    (fn []
      (reset! state {:ready? false}))))


(defn ready?
  [{:keys [ready?]}]
  ready?)

