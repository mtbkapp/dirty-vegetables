(ns dirty-vegetables.fauna
  (:require [clojure.set :as sets]
            [clojure.walk :as walk]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!
                               oget+ oset!+ ocall+ oapply+ ocall!+ oapply!+]]
            ["faunadb" :as f]))


(def db-key (atom nil))


(defn test-key
  [fk]
  (ocall (f/Client. #js {:secret fk})
         "query"
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
               (if (oget resp "ok")
                 (-> (ocall resp "json")
                     (.then (fn [body]
                              (js/console.log "got fuana key")
                              (js/console.log body)
                              (reset! db-key (oget body "faunaKey")))))
                 (js/console.error "non 200 trying to get fauna key"))))))


(defn fetch-fauna-key
  []
  (js/console.log "attempting to fetch fauna key")
  (if-let [user (js/netlifyIdentity.currentUser)]
    (-> (ocall user "jwt")
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


(defn ingredient-payload
  [ingredient]
  #js {:data (clj->js (walk/stringify-keys (dissoc ingredient :db/id)))})


(defn update-ingredient-query
  [{:keys [db/id] :as ingredient}]
  (f/query.Update
    (f/query.Ref (f/query.Collection "ingredients") id)
    (ingredient-payload ingredient)))


(defn create-ingredient-query
  [ingredient]
  (f/query.Create
    (f/query.Collection "ingredients")
    (ingredient-payload ingredient)))


(defn save-ingredient*
  [query]
  (js/Promise.
    (fn [resv rej]
      (if-let [fk @db-key]
        (-> (ocall (f/Client. #js {:secret fk}) "query" query)
            (.then resv)
            (.catch (fn [err]
                      (js/console.error err)
                      (rej ::save-error))))
        (rej ::no-key)))))


(defn save-ingredient
  [ingredient]
  (save-ingredient*
    (if (= (:db/id ingredient) (:db/id new-ingredient))
      (create-ingredient-query ingredient)
      (update-ingredient-query ingredient))))


(def ingredient-key-map
  {:id :db/id
   :name :ingredient/name
   :calorie-density :ingredient/calorie-density
   :volume :unit.type/volume 
   :mass :unit.type/mass
   :quantity :unit.type/quantity
   :measurement :calorie-density/measurement
   :calories :calorie-density/calories})


(defn from-fauna
  [resp]
  (let [data (oget resp "data")]
    (js->clj
      (cond (object? data)
            (let [id (oget (oget resp "ref") "id")]
              (aset data "id" id)
              data)
            (array? data)
            (ocall data
                   "map"
                   (fn [x]
                     (let [id (oget (oget x "ref") "id")
                           data (oget x "data")]
                       ; TODO figure out why I can't use oset!
                       (aset data "id" id)
                       data)))
            :else
            data)
      :keywordize-keys true)))


(defn key-map-xform
  [key-map]
  (fn [data]
    (walk/postwalk
      (fn [x]
        (if (map? x)
          (sets/rename-keys x key-map)
          x))
      data)))


(defn read-data
  [query xform]
  (js/Promise.
    (fn [resv rej]
      (if-let [fk @db-key]
        (-> (ocall (f/Client. #js {:secret fk}) "query" query)
            (.then (fn [resp]
                     (resv (xform (from-fauna resp)))))
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
  (read-data all-ingredients-query (key-map-xform ingredient-key-map)))


(defn add-default-densities
  [ingredient]
  (update-in ingredient
             [:data :ingredient/calorie-density]
             #(merge default-densities %)))


(defn read-single-ingredient
  [id]
  (if (= id "new")
    (js/Promise.resolve new-ingredient)
    (read-data (f/query.Get (f/query.Ref (f/query.Collection "ingredients") id))
               (comp add-default-densities
                     (key-map-xform ingredient-key-map)))))
