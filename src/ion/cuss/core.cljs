(ns ion.cuss.core
  (:refer-clojure :exclude [+ - * /])
  (:require-macros
   [ion.cuss.macros :refer [defbreakpoint]]
   [garden.def :refer [defcssfn defkeyframes defrule defstyles defstylesheet]])
  (:require
   [garden.arithmetic :refer [+ - * /]]
   [garden.color :as color :refer [hsl rgb]]
   [garden.core :refer [css]]
   [garden.stylesheet :refer [at-media]]
   [garden.units :as u :refer [em pt px]]))


;; -----------------------------------------------------------------------------
;; CSS Utilities

(defbreakpoint small-screen
  {:screen true
   :min-width (px 320)
   :max-width (px 480)})

(defbreakpoint medium-screen
  {:screen true
   :min-width (px 481)
   :max-width (px 1023)})

(defbreakpoint large-screen
  {:screen true
   :min-width (px 1024)})

;; (css
;;  [:.container
;;   (small-screen
;;    [:& {:max-width (px 480)}])
;;   (medium-screen
;;    [:& {:max-width (px 760)}])
;;   (large-screen
;;    [:& {:max-width (px 1224)}])])

(defrule article :article)
(defrule aside :aside)
(defrule body :body)
(defrule footer :footer)
(defrule header :header)
(defrule html :html)
(defrule main :main)

(defrule headings :h1 :h2 :h3)
(defrule sub-headings :h4 :h5 :h6)

(defrule ordered-list :ol)
(defrule unordered-list :ul)

(defrule active-links :a:active)
(defrule links :a:link)
(defrule on-hover :&:hover)
(defrule visited-links :a:visited)

;; (def center-text {:text-align "center"})

;; (def clearfix
;;   ["&" {:*zoom 1}
;;    ["&:before" "&:after" {:content "\"\"" :display "table"}]
;;    ["&:after" {:clear "both"}]])

;; (def gutter (px 20))

;; (def alegreya ["Alegreya" "Baskerville" "Georgia" "Times" "serif"])
;; (def mono ["Inconsolata" "Menlo" "Courier" "monospace"])
;; (def sans ["\"Open Sans\"" "Avenir" "Helvetica" "sans-serif"])
;; (def sans-serif '[helvetica arial sans-serif])

;; (defrule center :div.center)
;; (defrule top :section#top)
;; (defrule main :section#main)
;; (defrule sidebar :section#sidebar)

;; (def palette
;;   (let [base-color (hsl 0 100 50)]
;;     (color/shades base-color)))
