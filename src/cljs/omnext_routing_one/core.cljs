(ns omnext-routing-one.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

;; URL helper functions

(defn parse-url-hash [h]
  (let [
      h (subs h 2)
      [path params] (string/split h #"\u003f")
      path (string/split path \/) ; TODO: filter identity ???
      params (into {} (map (comp
                    (fn [[k v]] [(keyword (js/decodeURIComponent k))
                                 (if v
                                   (js/decodeURIComponent v)
                                   v
                                   )
                                 ])
                    #(string/split % "=")) (string/split params "&")))
      ]
  {:path path
   :params params}
  )
  )


;; Tab components

(defui CountriesTab
  static om/IQueryParams
  (params [_]
          ; ...
          )

  static om/IQuery
  (query [_]
         '[:countries {:page ?page}]
         )

  Object
  (render [this]
          (apply dom/ul nil
                 (map (fn [country]
                        (dom/li nil country)
                        )
                      )
                 )
          )
  )

(defui OceansTab
  static om/IQueryParams
  (params [_]
          ; ...
          )

  static om/IQuery
  (query [_]
         '[:oceans-tab]
         )

  Object
  (render [this]
          (apply dom/ul nil
                 (map (fn [country]
                        (dom/li nil country)
                        )
                      )
                 )
          )
  )

(defui HelloTab
  static om/IQueryParams
  (params [_]
          ; ...
          )

  static om/IQuery
  (query [_]
         '[:countries {:page ?page}]
         )

  Object
  (render [this]
          (apply dom/ul nil
                 (map (fn [country]
                        (dom/li nil country)
                        )
                      )
                 )
          )
  )

;; Tabs list

(def tabs
  [
   {:key :countries
    :url ["countries"]
    :title "Countries"
    :component CountriesTab
    }
   {:key :oceans
    :url ["oceans"]
    :title "Oceans"
    :component OceansTab
    }
   {:key :hello
    :url ["hello-world"]
    :title "Just «Hello, World!» in it"
    :component HelloTab
    }
   ]
  )

;; Processing the initial URL

(def initial-url
  js/window.location.hash)

(

;; Components

(defui RootView
  static om/IQueryParams
  (params [_]
    )

  static om/IQuery
  (query [_]
    )

  Object
  (render [this]
    (dom/div #js {:className "container"}
             "Hello, world!"
             )
          )
  )

;; Read & Write

(defmulti readf om/dispatch)

(defmethod readf :default
  [{:keys [state] :as env} k params]
  )

(defmulti mutatef om/dispatch)

;; Root

(def data {
           :countries (js->clj js/COUNTRIES_LIST :keywordize-keys true)
           :oceans ["Pacific" "Atlantic" "Indian" "Southern" "Arctic"]
           :hello "Hello, world! What's up?"
           })

(def parser (om/parser {:read readf
                        :mutate mutatef}))

(def reconciler (om/reconciler
                  {:state data
                   :parser parser
                   }
                  ))

(om/add-root!
  reconciler
  RootView
  (gdom/getElement "app"))

