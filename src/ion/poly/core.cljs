(ns ion.poly.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core :as cljs]
   [cljs.core.async :refer [<! >! chan close! put! sliding-buffer timeout]]
   [clojure.string :as string]
   [goog]
   [goog.date.Date]
   [goog.date.DateTime]
   [goog.date.UtcDateTime]
   [goog.dom :as dom]
   [goog.dom.classes :as classes]
   [goog.events :as events]
   [goog.string]
   [goog.style]
   [goog.userAgent])
  (:import
   [goog.dom ViewportSizeMonitor]
   [goog.events EventType]
   [goog Timer]))

(enable-console-print!)


;; -----------------------------------------------------------------------------
;; The top-Level goog namespace has properties and functions worth knowing about.

(comment
  goog/global
  goog/global.COMPILED
  goog.DEBUG
  goog.LOCALE
  goog.TRUSTED_SITE
  goog.STRICT_MODE_COMPATIBLE
  goog.DISALLOW_TEST_ONLY_CODE
  goog.ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING
  (goog/now)
)


;; -----------------------------------------------------------------------------
;; String Helpers

(defn html-escape
  ([s]
   (goog.string/htmlEscape s))
  ([s is-likely-to-contain-html-chars?]
   (goog.string/htmlEscape s is-likely-to-contain-html-chars?)))

(defn regexp-escape [s]
  (goog.string.regExpEscape s))

(defn whitespace-escape [s xml?]
  (goog.string.whitespaceEscape s xml?))


;; -----------------------------------------------------------------------------
;; Date and Time (For additional functionality use the cljs-time library:
;;                https://github.com/andrewmcveigh/cljs-time)

(defn js-now [] (js/Date.))

(defn now
  "Returns a DateTime for the current instant in the UTC time zone."
  []
  (goog.date.UtcDateTime.))

(defn time-now
  "Returns a local DateTime for the current instant without date or time zone
  in the current time zone."
  []
  (goog.date.DateTime.))

(defn today
  "Constructs and returns a new local DateTime representing today's date.
  local DateTime objects do not deal with timezones at all."
  []
  (goog.date.Date.))


;; -----------------------------------------------------------------------------
;; DOM

(defn get-viewport-size []
  (dom/getViewportSize))

(defn get-viewport-width []
  (.-width (dom/getViewportSize)))

(defn get-viewport-height []
  (.-height (dom/getViewportSize)))

(defn get-document-height []
  (dom/getDocumentHeight))

(defn get-document-scroll-x []
  (.-x (dom/getDocumentScroll)))

(defn get-document-scroll-y []
  (.-y (dom/getDocumentScroll)))

(defn get-root []
  (aget (dom/getElementsByTagNameAndClass "html") 0))

(defn get-body []
  (aget (dom/getElementsByTagNameAndClass "body") 0))

(defn get-document []
  (dom/getDocument))

(defn get-element [id]
  (dom/getElement (name id)))

(defn get-elements-by-tag-name-and-class
  ([tag-name]
   (dom/getElementsByTagNameAndClass (name tag-name)))
  ([tag-name class-name]
   (dom/getElementsByTagNameAndClass (name tag-name) (name class-name))))

(def request-animation-frame
  (or
   (.-requestAnimationFrame js/window)
   (.-webkitRequestAnimationFrame js/window)
   (.-mozRequestAnimationFrame js/window)
   (.-msRequestAnimationFrame js/window)
   (.-oRequestAnimationFrame js/window)
   (let [t0 (.getTime (js/Date.))]
     (fn [f]
       (js/setTimeout
        #(f (- (.getTime (js/Date.)) t0))
        16.66666)))))

;; (defn set-stylesheet! [stylesheet]
;;   (let [el (.createElement js/document "style")
;;         node (.createTextNode js/document stylesheet)]
;;     (.appendChild el node)
;;     (.appendChild (.-head js/document) el)))

(defn set-title! [title]
  (set! (.-title js/document) title))


;; -----------------------------------------------------------------------------
;; Style

(defn install-styles! [styles]
  (goog.style/installStyles styles))

(defn get-page-offset [element]
  (let [coord (goog.style/getPageOffset element)]
    {:x (.-x coord) :y (.-y coord)}))


;; -----------------------------------------------------------------------------
;; JavaScript Interop Helpers

(defn- camel-case [head & more]
  (cons head (map string/capitalize more)))

(defn k->camel-case-name [k]
  "Return a camelCase string version of a possibly hyphenated keyword."
  (->> (string/split (name k) "-") (apply camel-case) string/join))

(defn ks->obj-prop-m [ks]
  "Return an object property map for a coll of keywords."
  (into {} (for [k ks] {k (k->camel-case-name k)})))

(defn o->m [m o]
  "Return a map containing the properties of an object based on an object
  property map."
  (into {} (for [k (keys m)] {k (aget o (k m))})))


;; -----------------------------------------------------------------------------
;; Event Helpers

(defn k->event-type [k]
  (aget EventType (-> (name k) (string/replace "-" "") string/upper-case)))

(defn listen!
  [src event-type func]
  (let [event-type (if (keyword? event-type) (k->event-type event-type) event-type)]
    (events/listen src event-type func)))

(defn listen-once!
  [src event-type func]
  (let [event-type (if (keyword? event-type) (k->event-type event-type) event-type)]
    (events/listenOnce src event-type func)))

(defn listen-put!
  ([src event-type channel]
   (let [listener-key (listen! src event-type #(put! channel %))]
     [channel listener-key]))
  ([src event-type channel subject]
   (let [listener-key (listen! src event-type #(put! channel subject))]
     [channel listener-key])))

(defn listen-take!
  [channel func]
  (go-loop []
    (when-let [taken (<! channel)]
      (func taken)
      (recur))))

(defn unlisten! [key]
  (events/unlistenByKey key))

(def e-ks
  #{:alt-key
    :button
    :buttons
    :client-x
    :client-y
    :ctrl-key
    :current-target
    :default-prevented
    :detail
    :event-phase
    :key-cde
    :meta-key
    :offset-x
    :offset-y
    :related-target
    :screen-x
    :screen-y
    :shift-key
    :state
    :target
    :type})

;; goog.events.BrowserEvent.MouseButton = {
;;   LEFT: 0,
;;   MIDDLE: 1,
;;   RIGHT: 2


;; -----------------------------------------------------------------------------
;; Mouse Events

(def e-ks-mouse
  #{:alt-key
    :button
    :buttons
    :client-x
    :client-y
    :ctrl-key
    :detail
    :event-phase
    :meta-key
    :screen-x
    :screen-y
    :shift-key})

(def e-ks-not-mouse-move
  #{:buttons})

(def e-ks-mouse-move (disj e-ks-mouse e-ks-not-mouse-move))

(def e-mouse-move->m (partial o->m (ks->obj-prop-m e-ks-mouse-move)))

(defn channel-for-mouse-move!
  ([src]
   (channel-for-mouse-move! src (sliding-buffer 1)))
  ([src buffer]
   (listen-put! src :mouse-move (chan buffer (map e-mouse-move->m)))))


;; -----------------------------------------------------------------------------
;; Viewport Resize Event

(defn viewport-size-monitor->m [monitor]
  (let [size (.getSize monitor)
        h (. size -height)
        w (. size -width)]
    {:height h
     :width w}))

(defn listen-for-viewport-resize! [func]
  (let [monitor (ViewportSizeMonitor.)]
    (listen! monitor :resize #(func (viewport-size-monitor->m monitor)))))

(defn listen-put-viewport-resize! [channel]
  (let [monitor (ViewportSizeMonitor.)]
    (listen-put! monitor :resize channel monitor)))

(defn channel-for-viewport-resize!
  ([]
   (channel-for-viewport-resize! (sliding-buffer 1)))
  ([buffer]
   (listen-put-viewport-resize! (chan buffer (map viewport-size-monitor->m)))))


;; -----------------------------------------------------------------------------
;; Window Events

;; (defn listen-for-window-load! [func]
;;   (listen! js/window :load func))


