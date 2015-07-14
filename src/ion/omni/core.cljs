(ns ion.omni.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core :as cljs]
   [cljs.core.async :refer [<! >! chan close! put! sliding-buffer timeout]]))

;(enable-console-print!)


(defn spin [& opts]
  ;(prn "omni/spin" opts)
  true)
