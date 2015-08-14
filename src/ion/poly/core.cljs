(ns ion.poly.core
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core :as cljs]
   [cljs.core.async :refer [<! >! chan close! put! sliding-buffer timeout]]
   [clojure.string :as string]
   [goog.async.AnimationDelay]
   [goog.async.nextTick]
   [goog.date.Date]
   [goog.date.DateTime]
   [goog.date.UtcDateTime]
   [goog.dom :as dom]
   [goog.dom.classes :as classes]
   [goog.events :as events]
   [goog.object]
   [goog.string]
   [goog.style]
   [goog.userAgent]
   [phalanges.core :as phalanges])
  (:import
   [goog.dom ViewportSizeMonitor]
   [goog.events EventType KeyHandler]
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
;; String Helpers - For additional functionality use the cuerdas library:
;;                  https://github.com/funcool/cuerdas

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
;; Date and Time - For additional functionality use the cljs-time library:
;;                 https://github.com/andrewmcveigh/cljs-time

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
  (goog.object/get (dom/getElementsByTagNameAndClass "html") 0))

(defn get-body []
  (goog.object/get (dom/getElementsByTagNameAndClass "body") 0))

(defn get-document []
  (dom/getDocument))

(defn get-element [id]
  (dom/getElement (name id)))

(defn get-elements-by-tag-name-and-class
  ([tag-name]
   (dom/getElementsByTagNameAndClass (name tag-name)))
  ([tag-name class-name]
   (dom/getElementsByTagNameAndClass (name tag-name) (name class-name))))

(defn set-title! [title]
  (set! (.-title js/document) title))


;; -----------------------------------------------------------------------------
;; Style

(defn install-styles! [styles]
  (goog.style/installStyles styles))

;; (defn set-stylesheet! [stylesheet]
;;   (let [el (.createElement js/document "style")
;;         node (.createTextNode js/document stylesheet)]
;;     (.appendChild el node)
;;     (.appendChild (.-head js/document) el)))

;; (defn get-page-offset [element]
;;   (let [coord (goog.style/getPageOffset element)]
;;     {:x (.-x coord) :y (.-y coord)}))


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
  (into {} (for [k (keys m)] {k (goog.object/get o (k m))})))


;; -----------------------------------------------------------------------------
;; Async Helpers

(defn next-tick!
  [callback]
  (goog.async.nextTick callback))

(defn request-animation-frame!
  "A delayed callback that pegs to the next animation frame."
  [callback]
  (.start (goog.async.AnimationDelay. callback)))

(defn animation-frame-loop!
  ([callback]
   (animation-frame-loop! callback true))
  ([callback activate?]
   (let [alive? (atom true)
         active? (atom activate?)]
     (letfn [(step
               [timestamp]
               (when @alive?
                 (request-animation-frame! step)
                 (when @active?
                   (callback timestamp))))]
       (request-animation-frame! step))
     [alive? active?])))

(defn listen-animation-frame!
  [callback]
  (letfn [(step
           [timestamp]
           (when (callback timestamp) (request-animation-frame! step)))]
    (request-animation-frame! step)))

(defn listen-fps!
  "Executes callback at every frame returning the frames-per-second."
  ([callback]
   (request-animation-frame! (listen-fps! callback nil)))
  ([callback previous]
   (letfn [(step
            [timestamp]
            (let [previous (or previous (- timestamp 17))
                  elapsed (- timestamp previous)
                  fps (->> (/ elapsed 1000) (/ 1) (.floor js/Math))]
              (if (callback fps)
                (request-animation-frame! (listen-fps! callback timestamp)))))]
     step)))

(defn listen-fps-interval!
  "Executes callback at regular intervals returning the frames-per-second."
  ([callback]
   (listen-fps-interval! callback 500)) ; Measure every half-second
  ([callback interval]
   (request-animation-frame! (listen-fps-interval! callback interval nil 1)))
  ([callback interval start-time frame-count]
   (letfn [(step
            [timestamp]
            (let [start-time (or start-time (- timestamp 17))
                  elapsed (- timestamp start-time)]
              (if (< elapsed interval)
                (request-animation-frame!
                 (listen-fps-interval! callback interval start-time (inc frame-count)))
                (let [fps (->> (/ frame-count elapsed) (* 1000) (.floor js/Math))]
                  (if (callback fps)
                    (request-animation-frame!
                     (listen-fps-interval! callback interval timestamp 1)))))))]
     step)))

(defn listen-next-tick!
  [callback]
  (letfn [(step [] (if (callback) (goog.async.nextTick step)))]
    (goog.async.nextTick step)))

;; (defn foldp! [func init in]
;;   (let [out (chan)]
;;     (put! out init)
;;     (go-loop [m init
;;               v (<! in)]
;;       (let [m2 (func m v)]
;;         (put! out m2)
;;         (recur m2 (<! in))))
;;     out))

(defn take-back!
  "A delayed callback that pegs to the next message on the channel."
  [channel callback]
  (go-loop []
    (when-let [taken (<! channel)]
      (callback taken)
      (recur))))


;; -----------------------------------------------------------------------------
;; Event Helpers

(defn event-type [e-type]
  "Returns a normalized form of the event type string, keyword, or symbol."
  (let [e-type (-> (name e-type) (string/replace "-" "") string/upper-case)]
    (or (goog.object/get EventType e-type) (string/lower-case e-type))))

(defn event-source [src e-type]
  "Returns a wrapped-if-necessary event source."
  (let [e-type (event-type e-type)]
    (if (= "key" e-type)
      (KeyHandler. src)
      src)))

(defn- listen-base!
  [func src e-type callback]
  (let [e-type (event-type e-type)]
    (func (event-source src e-type) e-type callback)))

(def listen! (partial listen-base! events/listen))

(def listen-once! (partial listen-base! events/listenOnce))

(declare e-chan keyboard-e-chan mouse-e-chan)

(defn e-type->chan [e-type]
  (condp contains? (event-type e-type)
    #{"key"} (keyboard-e-chan)
    #{"mouseclick" "mousedown" "mousemove" "mouseup"} (mouse-e-chan)))

(defn listen-put!
  ([src e-type]
   (listen-put! src e-type (e-type->chan e-type)))
  ([src e-type channel]
   (let [listener-key (listen! src e-type #(put! channel %))]
     [listener-key channel]))
  ([src e-type channel subject]
   (let [listener-key (listen! src e-type #(put! channel subject))]
     [listener-key channel])))

(defn unlisten!
  ([listener-key]
   (events/unlistenByKey listener-key))
  ([listener-key channel]
   (events/unlistenByKey listener-key)
   (close! channel)))

(defn e-plus [mapper]
  (let [counter (atom 0)]
    (fn [event]
      (let [m (mapper event)]
        (assoc m :poly/count (swap! counter inc))))))

(defn prevent-default
  [event]
  (.preventDefault event))

(defn stop-propagation
  [event]
  (.stopPropagation event))


;; -----------------------------------------------------------------------------
;; Browser Event (Generic event that's more of a base pattern than anything useful)

(def event-ks
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

(def e->m (partial o->m (ks->obj-prop-m event-ks)))

(defn e-chan
  ([]
   (e-chan (sliding-buffer 1)))
  ([buffer]
   (chan buffer (map (e-plus e->m)))))


;; -----------------------------------------------------------------------------
;; Keyboard Events (:key :key-down :key-press :key-up)

(def keyboard-event-ks
  #{:alt-key
    :char-code
    :ctrl-key
    :current-target
    :default-prevented
    :is-composing
    :key-code
    :meta-key
    :related-target
    :repeat
    :shift-key
    :target
    :type})

(def keyboard-e->m (partial o->m (ks->obj-prop-m keyboard-event-ks)))

(defn keyboard-plus [m]
  (assoc m :poly/keyword (phalanges/keycode->keyword (:key-code m))))

(defn keyboard-e-chan
  ([]
   (keyboard-e-chan (sliding-buffer 1)))
  ([buffer]
   (chan buffer (map (comp keyboard-plus (e-plus keyboard-e->m))))))


;; -----------------------------------------------------------------------------
;; Mouse Events

(def mouse-event-ks
  #{:alt-key
    :button
    :buttons
    :client-x
    :client-y
    :ctrl-key
    :current-target
    :default-prevented
    :detail
;    :event-phase
;    :key-cde
    :meta-key
    :offset-x
    :offset-y
    :related-target
    :screen-x
    :screen-y
    :shift-key
;    :state
    :target
    :type})

(def mouse-e->m (partial o->m (ks->obj-prop-m mouse-event-ks)))

(defn mouse-e-chan
  ([]
   (mouse-e-chan (sliding-buffer 1)))
  ([buffer]
   (chan buffer (map (e-plus mouse-e->m)))))


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


