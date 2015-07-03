(ns ion.cuss.macros
  (:require
   [garden.stylesheet :refer [at-media]]))

(defmacro defbreakpoint [name media-params]
  `(defn ~name [& rules#]
     (at-media ~media-params
       [:& rules#])))
