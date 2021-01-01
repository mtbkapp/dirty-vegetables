(ns dirty-vegetables.fauna
  (:require [clojure.set :as sets]
            [clojure.walk :as walk]
            ["faunadb" :as f]))


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
                              (js/console.log body)
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
    (js/console.log ">>>>" new-key)
    (if (nil? new-key)
      (cb :logout)
      (cb :login))))


(defn init!
  [cb]
  (js/console.log "init fauna")
  (add-watch db-key :cb (on-key-change cb))
  (reset! db-key (get-ls-key))
  (attach-identity-events))


(defn save-ingredient
  [ingredient]
  (js/Promise.
    (fn [resv rej]
      (if-let [fk @db-key]
        (-> (.query (f/Client. #js {:secret fk})
                    (f/query.Create
                      (f/query.Collection "ingredients")
                      #js {:data (clj->js (walk/stringify-keys ingredient))}))
            (.then resv)
            (.catch (fn [err]
                      (js/console.error err)
                      (rej ::save-error))))
        (rej ::no-key)))))


(def ingredient-key-map
  {"name" :ingredient/name
   "calorie-density" :ingredient/calorie-density
   "volume" :unit.type/volume 
   "mass" :unit.type/mass
   "quantity" :unit.type/quantity
   "measurement" :calorie-density/measurement
   "calories" :calorie-density/calories})


(defn from-fauna
  [resp key-map]
  (walk/postwalk
    (fn [x]
      (if (map? x)
        (sets/rename-keys x key-map)
        x))
    (js->clj (.map (.-data resp)
                   (fn [x]
                     #js {"id" (.-id (.-ref x))
                          "data" (.-data x)})))))


(defn read-data
  [query]
  (js/Promise.
    (fn [resv rej]
      (if-let [fk @db-key]
        (-> (.query (f/Client. #js {:secret fk}) query)
            (.then (fn [resp]
                     (resv (from-fauna resp ingredient-key-map))))
            (.catch (fn [err]
                      (js/console.error err)
                      (rej ::read-error))))
        (rej ::no-key)))))


(def all-ingredients-query
  (f/query.Map
    (f/query.Paginate 
      (f/query.Documents
        (f/query.Collection "ingredients"))
      #js {:size 10000})
    (f/query.Lambda "x" (f/query.Get (f/query.Var "x")))))


(defn fetch-all-ingredients
  []
  (read-data all-ingredients-query))
