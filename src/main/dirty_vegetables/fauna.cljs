(ns dirty-vegetables.fauna
  (:require ["faunadb" :as f]))



(defn test-key
  [fk]
  (.query (f/Client. #js {:secret fk})
          (f/query.Create
            (f/query.Collection "test")
            #js {:data #js {:x (js/Math.random)}})))
