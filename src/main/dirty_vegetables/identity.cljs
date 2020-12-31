(ns dirty-vegetables.identity
  (:require [reagent.core :as r]
            [dirty-vegetables.fauna :as fauna]))


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
                              (js/console.log "got fuana key")
                              (swap! state 
                                     assoc
                                     :ready? true
                                     :fauna-key (.-faunaKey ^object body)))))
                 (js/console.error "non 200 trying to get fauna key"))))))


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


(def manual-fauna-key-key "manualFaunaKey")


(def manual-fauna-key-state
  (r/atom {:key (str (js/localStorage.getItem manual-fauna-key-key))
           :error nil}))


(defn on-manual-fauna-key-click
  [{fk :key}]
  (js/localStorage.setItem manual-fauna-key-key fk)
  (-> (fauna/test-key fk)
      (.then (fn []
               (swap! state assoc :ready? true :fauna-key fk)))
      (.catch (fn []
                (swap! manual-fauna-key-state assoc :error "Invalid fauna key")))))


(defn manual-fauna-key
  []
  (let [{fk :key error :error} @manual-fauna-key-state]
    [:div {:id "manual-fauna-key"}
     [:label {:for "manual-fauna-key"} "Manual fauna key:"]
     [:input {:type "text"
              :id "manual-fauna-key"
              :value fk 
              :on-change #(swap! manual-fauna-key-state
                                 assoc
                                 :key
                                 (.-value (.-target %)))}]
     [:button {:type "button"
               :on-click #(on-manual-fauna-key-click @manual-fauna-key-state)}
      "Ok"]
     [:span {:class "error"} error]]))

