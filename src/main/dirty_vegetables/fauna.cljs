(ns dirty-vegetables.fauna
  (:require [clojure.set :as sets]
            [clojure.walk :as walk]
            [dirty-vegetables.core :as core]
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


(defn write-payload
  [rec]
  #js {:data (clj->js (walk/stringify-keys (dissoc rec :db/id)))})


(defn update-rec-query
  [coll-name {:keys [db/id] :as rec}]
  (f/query.Update
    (f/query.Ref (f/query.Collection coll-name) id)
    (write-payload rec)))


(defn create-rec-query
  [coll-name rec]
  (f/query.Create
    (f/query.Collection coll-name)
    (write-payload rec)))


(defn save-rec*
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


(defn save-rec
  [coll-name rec]
  (save-rec*
    (if (= "new" (:db/id rec))
      (create-rec-query coll-name rec)
      (update-rec-query coll-name rec))))


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
                     (let [data (from-fauna resp)]
                       (resv (if (map? data)
                               (xform data)
                               (mapv xform data))))))
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
  (update ingredient
          :ingredient/calorie-density
          #(merge core/default-densities %)))


(defn fetch-single-ingredient
  [id]
  (if (= id (:db/id core/new-ingredient))
    (js/Promise.resolve core/new-ingredient)
    (read-data (f/query.Get (f/query.Ref (f/query.Collection "ingredients") id))
               (comp add-default-densities
                     (key-map-xform ingredient-key-map)))))


(def all-recipes-query
  (f/query.Map
    (f/query.Paginate
      (f/query.Documents
        (f/query.Collection "recipes"))
      #js {:size 10000})
    (f/query.Lambda "x" (f/query.Get (f/query.Var "x")))))


(def recipe-key-map
  {:name :recipe/name
   :notes :recipe/notes
   :ingredients :recipe/ingredients
   :totals :recipe/totals
   :id :ingredient/id
   :measurement :input/measurement
   :mass :unit.type/mass
   :volume :unit.type/volume
   :quantity :unit.type/quantity})


(defn fix-recipe-id
  [{:keys [ingredient/id] :as recipe}]
  (-> recipe
      (dissoc recipe :ingredient/id)
      (assoc recipe :db/id id)))


(def recipe-xform
  (comp #(sets/rename-keys % {:ingredient/id :db/id})
        (key-map-xform recipe-key-map)))


(defn fetch-all-recipes
  []
  (read-data all-recipes-query recipe-xform))


(defn fetch-single-recipe
  [id]
  (if (= id (:db/id core/new-recipe))
    (js/Promise.resolve core/new-recipe)
    (read-data (f/query.Get (f/query.Ref (f/query.Collection "recipes") id))
               recipe-xform)))


(defn fetch-recipe-and-all-ingredients
  [id]
  (js/Promise.
    (fn [resv rej]
      (-> (js/Promise.all
            #js [(fetch-single-recipe id) (fetch-all-ingredients)])
          (.then (fn [arr]
                   (resv {:recipe (aget arr 0)
                          :ingredients (aget arr 1)})))
          (.catch (fn [err]
                    (js/console.error "Error in fetch-recipe-and-all-ingredients" err)
                    (rej err)))))))



