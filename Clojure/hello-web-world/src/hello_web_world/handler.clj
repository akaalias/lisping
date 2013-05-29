(ns hello-web-world.handler
  (:use [compojure.core]
        [hiccup core page])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(def list-items (range 1 100))

(defn index-page []
  (html5
    [:head
      [:title "Hello World"]
      (include-css "/css/style.css")]
    [:body
      [:h1 "Hello World Sorting does matter?"]]))

(defroutes app-routes
  (GET "/" [] (index-page))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))

