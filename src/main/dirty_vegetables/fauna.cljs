(ns dirty-vegetables.fauna
  (:require ["faunadb" :as f]))


(def db-key (atom nil))


(defn test-key
  [fk]
  (.query (f/Client. #js {:secret fk})
          (f/query.Create
            (f/query.Collection "test")
            #js {:data #js {:x (js/Math.random)}})))


(def ls-key "manualFaunaKey")


(defn get-ls-key
  []
  (js/localStorage.getItem ls-key))


(defn set-ls-key
  [k]
  (js/localStorage.setItem ls-key k))


(defn reset-manaul-key
  []
  (let [new-key (js/prompt "Fuana Key")]
    (cond (= "" new-key) (do (set-ls-key nil)
                             (reset! db-key nil))
          (some? new-key) (-> (test-key new-key)
                              (.then (fn []
                                       (js/console.log "Valid fauna key")
                                       (reset! db-key new-key)
                                       (set-ls-key new-key)))
                              (.catch (fn []
                                        (js/console.error "Invalid fauna key")))))))


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
                              (reset! db-key (.-faunaKey ^object body)))))
                 (js/console.error "non 200 trying to get fauna key"))))))


(defn fetch-fauna-key
  []
  (js/console.log "attempting to fetch fauna key")
  (if-let [user (js/netlifyIdentity.currentUser)]
    (-> (.jwt user)
        (.then fetch-fauna-key*)
        (.catch (fn [err]
                  (js/console.error "error getting netlify jwt" err))))
    (js/console.error "error getting fauna key: not logged in")))


(defn attach-identity-events
  []
  (js/netlifyIdentity.on "login" fetch-fauna-key)
  (js/netlifyIdentity.on "logout" #(reset! db-key nil)))


(defn on-key-change
  [cb]
  (fn [_ _ old-key new-key]
    (if (nil? new-key)
      (cb :logout)
      (cb :login))))


(defn init!
  [cb]
  (js/console.log "init fauna")
  (add-watch db-key :cb (on-key-change cb))
  (reset! db-key (get-ls-key))
  (attach-identity-events))
