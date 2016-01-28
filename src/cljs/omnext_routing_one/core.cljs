(ns omnext-routing-one.core
  (:require-macros
    [cljs.core.async.macros :refer (go-loop)]
    )
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [clojure.string :as string]
            [cljs.core.async :refer (<! put! chan)]
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
   (path-and-params-to-hash path nil)
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

(defn get-query-and-params-by-parsed-url [tabs parsed-url]
  (if-let [component (some->>
    (get-tab-by-parsed-url tabs parsed-url)
    :component
    )]
      {:query (om/query component)
       :params (let [component-params (om/params component)]
                 (merge
                   component-params
                   (select-keys
                     (:params parsed-url)
                     (keys component-params)
                     )))
       }
    )
  )


;; Tab components

(defui CountriesTab
  static om/IQueryParams
  (params [_]
          {:page 1}
          )

  static om/IQuery
  (query [_]
         '[:tab-key/countries
           (:countries {:page ?page})]
         )

  Object
  (render [this]
          (apply dom/ul nil
                 (map (fn [country]
                        (dom/li nil (:name country))
                        )
                      (:countries (om/props this))
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
         '[:tab-key/oceans
           :oceans]
         )

  Object
  (render [this]
          (apply dom/ul nil
                 (map (fn [ocean]
                        (dom/li nil
                                ocean
                                )
                        )
                      (:oceans (om/props this))
                      )
                 )
          )
  )

(defui HelloTab
  static om/IQueryParams
  (params [_]
          ; ...
          {}
          )

  static om/IQuery
  (query [_]
         '[:tab-key/hello
           :hello]
         )

  Object
  (render [this]
          (dom/div nil
          "something"
               )
          (dom/h1 nil
                  (:hello (om/props this))
                  )
          )
  )

;; Tabs list

(def tabs
  [
   {:key :countries
    :url-path ["countries"]
    :title "Countries"
    :component CountriesTab
    }
   {:key :oceans
    :url-path ["oceans"]
    :title "Oceans"
    :component OceansTab
    }
   {:key :hello
    :url-path ["hello-world"]
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

(def initial-query-and-params
  (get-query-and-params-by-parsed-url
    tabs
    initial-url)
  )

(def initial-query
  (:query initial-query-and-params)
  )

(def initial-params
  (:params initial-query-and-params)
  )

;; Components


(def routes-chan (chan))

(defui RootView
  static om/IQueryParams
  (params [_]
          initial-params ; here and below, we're passing (pre-computed) value
                         ; and not doing function calls
    )

  static om/IQuery
  (query [_]
         initial-query
    )

  Object
  (componentDidMount [this]
                     (go-loop [new-query (<! routes-chan)]
                                (om/set-query!
                                  this
                                  new-query
                                  )
                              (recur (<! routes-chan))
                       )
                     )

  (render [this]
          (println "render called. and"
                       (meta (first (-> this om/get-query)))
                       (-> this om/get-query)
                   (om/query this)
                   (om/params this)
                   )
          (let [;unbound-query (om/query this)
                ;unbound-params (om/params this)
                query (om/get-query this)
                tab-is-active (fn [tab]
                                (contains?
                                  (into #{} (comp
                                              (filter keyword?)
                                              (filter #(= (namespace %) "tab-key"))
                                              (map name)
                                              (map keyword)
                                             ) query)
                                   (:key tab)
                                   )
                                )
                ]
            (dom/div #js {:className "container"}
                     ;; Tabs (navigation)
                        (apply dom/ul
                        #js
                        {:className "nav nav-tabs", :role "tablist"}
                        (map (fn [tab]
                        (dom/li
                          #js
                          {:className (when (tab-is-active tab)
                                        "active"
                                        ), :role "presentation"}
                          (dom/a
                          #js
                          {:shape "rect",
                            :href (path-and-params-to-hash
                                    (:url-path tab)
                                    nil ; XXX: tabs not intended to have params yet
                                    ),
                            :aria-controls "home",
                            :role "tab",
                            }
                          (:title tab))))
                             tabs))
                     (dom/div nil
                              ; ...
                              (if-let [current-tab (first
                                                     (filter tab-is-active tabs)
                                                     )]
                                ; ...
                                (do
                                  (
                                    (om/factory
                                      (:component current-tab))
                                    (om/props this)
                                   )
                                  )

                                )
                       )
                    )
            )
          )
  )

;; Read & Write

(defmulti readf om/dispatch)

(defmethod readf :default
  [{:keys [state] :as env} k params]
  (println "readf" k params)
  (if-let [v (get @state k)]
    {:value v}
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




;; Watch the URL changes
(aset
  js/window
  "onhashchange"
  (fn [_]
    ; ...
    (let [parsed-url (parse-url-hash
          js/window.location.hash
          )
          new-query (get-query-and-params-by-parsed-url
        tabs
        parsed-url
        )
          ]
      (put! routes-chan new-query)
      )
    )
  )
