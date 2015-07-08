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
;; Event Helpers

(def event-type-keywords
  [:animation-end
   :animation-iteration
   :animation-start
   :before-copy
   :before-cut
   :before-paste
   :before-unload
   :blur
   :change
   :click
   :composition-end
   :composition-start
   :composition-update
   :connect
   :console-message
   :context-menu
   :copy
   :cut
   :dbl-click
   :deactivate
   :dom-attr-modified
   :dom-character-data-modified
   :dom-content-loaded
   :dom-node-inserted
   :dom-node-inserted-into-document
   :dom-node-removed
   :dom-node-removed-from-document
   :dom-subtree-modified
   :drag
   :drag-end
   :drag-enter
   :drag-leave
   :drag-over
   :drag-start
   :drop
   :error
   :exit
   :focus
   :focus-in
   :focus-out
   :got-pointer-capture
   :hash-change
   :help
   :input
   :key-down
   :key-press
   :key-up
   :load
   :load-abort
   :load-commit
   :load-redirect
   :load-start
   :load-stop
   :lose-capture
   :lost-pointer-capture
   :message
   :mouse-down
   :mouse-enter
   :mouse-leave
   :mouse-move
   :mouse-out
   :mouse-over
   :mouse-up
   :ms-gesture-change
   :ms-gesture-end
   :ms-gesture-hold
   :ms-gesture-start
   :ms-gesture-tap
   :ms-got-pointer-capture
   :ms-inertia-start
   :ms-lost-pointer-capture
   :ms-pointer-cancel
   :ms-pointer-down
   :ms-pointer-enter
   :ms-pointer-hover
   :ms-pointer-leave
   :ms-pointer-move
   :ms-pointer-out
   :ms-pointer-over
   :ms-pointer-up
   :offline
   :online
   :orientation-change
   :page-hide
   :page-show
   :paste
   :pointer-cancel
   :pointer-down
   :pointer-enter
   :pointer-leave
   :pointer-move
   :pointer-out
   :pointer-over
   :pointer-up
   :pop-state
   :property-change
   :ready-state-change
   :reset
   :resize
   :responsive
   :right-click
   :scroll
   :select
   :select-start
   :size-changed
   :storage
   :submit
   :text
   :text-input
   :touch-cancel
   :touch-end
   :touch-move
   :touch-start
   :transitionend
   :unload
   :unresponsive
   :visibility-change
   :wheel])

(defn keyword->event-string [k]
  (-> (dehyphenate-keyword k)
      (name)
      (string/upper-case)))

(defn dehyphenate-keyword [k]
  (-> (name k)
      (string/replace "-" "")
      (keyword)))

(def map-of-keyword->event-type
  (into {} (for [k event-type-keywords
                 :let [event-type (aget EventType (keyword->event-string k))]]
             {k event-type
              (dehyphenate-keyword k) event-type})))

(defn listen!
  [src event-type func]
  (events/listen src (get map-of-keyword->event-type event-type event-type) func))

(defn listen-take!
  [channel func]
  (go-loop []
    (when-let [taken (<! channel)]
      (func taken)
      (recur))))

(defn listen-put!
  ([src event-type channel]
   (listen! src event-type #(put! channel %))
   channel)
  ([src event-type channel subject]
   (listen! src event-type #(put! channel subject))
   channel))


;; -----------------------------------------------------------------------------
;; Mouse Events

(def base-mouse-event-keywords
  [:alt-key
   :buttons
   :client-x
   :client-y])

(defn extract-mouse-info [e]
  {:alt-key (.-altKey e)
   :button (.-button e)
;   :buttons (.-buttons e)
   :client-x (.-clientX e)
   :client-y (.-clientY e)
   :ctrl-key (.-ctrlKey e)
   :detail (.-detail e)
   :event-phase (.-eventPhase e)
   :meta-key (.-metaKey e)
   :screen-x (.-screenX e)
   :screen-y (.-screenY e)
   :shift-key (.-shiftKey e)
   })

(defn get-mouse-channel
  ([]
   (get-mouse-channel (sliding-buffer 1)))
  ([buffer]
   (chan buffer (map extract-mouse-info))))

(defn listen-put-mouse-move! [src channel]
  (listen-put! src :mouse-move channel))

(defn channel-for-mouse-move!
  ([src]
   (listen-put-mouse-move! src (get-mouse-channel)))
  ([src buffer]
   (listen-put-mouse-move! src (get-mouse-channel buffer))))


;; -----------------------------------------------------------------------------
;; Viewport Resize Event

(defn extract-viewport-size [monitor]
  (let [size (.getSize monitor)
        h (. size -height)
        w (. size -width)]
    {:height h
     :width w}))

(defn get-viewport-resize-channel
  ([]
   (get-viewport-resize-channel (sliding-buffer 1)))
  ([buffer]
   (chan buffer (map extract-viewport-size))))

(defn listen-put-viewport-resize! [channel]
  (let [monitor (ViewportSizeMonitor.)]
    (listen-put! monitor :resize channel monitor)))

(defn channel-for-viewport-resize!
  ([]
   (listen-put-viewport-resize! (get-viewport-resize-channel)))
  ([buffer]
   (listen-put-viewport-resize! (get-viewport-resize-channel buffer))))

(defn listen-for-viewport-resize! [func]
  (let [monitor (ViewportSizeMonitor.)]
    (listen! monitor :resize #(func (extract-viewport-size monitor)))))


;; -----------------------------------------------------------------------------
;; Window Events

;; (defn listen-for-window-load! [func]
;;   (listen! js/window :load func))


