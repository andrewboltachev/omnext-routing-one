(ns omnext-routing-one.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [clojure.string :as string]
            ))

(enable-console-print!)

;; URL helper functions

(defn parse-url-hash [h]
  "#/foo/bar/?a=b&c=d -> {:path [\"foo\" \"bar\"], :params {:a \"b\", :c \"d\"}}"
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

(defn path-and-params-to-hash
  "Opposite for function from above. Expects keys of `params` to be keywords"
  ([path]
   (path-and-params-to-hash nil)
   )
  ([path params]
    (apply str "#/" (string/join "/" path) "/"
         (when params
           (cons
             "?"
            (string/join "&" (map (fn [[k v]]
                                    (str
                                      (-> k name js/encodeURIComponent)
                                      "="
                                      (-> v js/encodeURIComponent)
                                      )
                                    ) params))
            )
           )
         )
   )
  )

;; Tab functions

(defn get-tab-by-parsed-url [tabs {:keys [path params]}]
  "Tabs format is defined in `tabs` below. See `parse-url-hash` for second param"
  (first (filter
           #(= (:url-path %) path)
           tabs))
  )


;; Tab components

(defui CountriesTab
  static om/IQueryParams
  (params [_]
          {:page 1}
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
                        (dom/li nil
                                (:name country)
                                (dom/span
                                  #js {:style #js {:fontWeight "bold"
                                                   :marginLeft "5px"}}
                                  "("
                                  (:code country)
                                  ")"
                                  )
                                )
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
                 (map (fn [ocean]
                        (dom/li nil ocean)
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
  (parse-url-hash
    js/window.location.hash
    )
  )

(def initial-tab
  (get-tab-by-parsed-url tabs initial-url)
  )

(def initial-component
  (:component initial-tab)
  )

(def initial-query-for-root
  (when initial-component
    (om/query initial-component)
    )
  )

(def initial-params-for-root
  (when initial-component
    (om/params initial-component)
    )
  )

;; Components

(defui RootView
  static om/IQueryParams
  (params [_]
          initial-params-for-root ; here and below, we're passing (pre-computed) value
                                  ; and not doing function calls
    )

  static om/IQuery
  (query [_]
         initial-query-for-root
    )

  Object
  (render [this]
          (let []
            (dom/div #js {:className "container"}
                     ;; Tabs (navigation)
                        (apply dom/ul
                        #js
                        {:className "nav nav-tabs", :role "tablist"}
                        (map (fn [tab]
                        (dom/li
                          #js
                          {:className "active", :role "presentation"}
                          (dom/a
                          #js
                          {:shape "rect",
                            :href "#home",
                            :aria-controls "home",
                            :role "tab",
                            :data-toggle "tab"}
                          (:title tab))))
                             tabs))
                    )
            )
          )
  )

;; Read & Write

(defmulti readf om/dispatch)

(defmethod readf :default
  [{:keys [state] :as env} k params]
  (if-let [v (get @state k)]
    {:value k}
    {:value nil}
    )
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

