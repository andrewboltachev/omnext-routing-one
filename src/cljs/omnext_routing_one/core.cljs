(ns omnext-routing-one.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

;; URL helper functions


;; Components

(defui RootView
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

