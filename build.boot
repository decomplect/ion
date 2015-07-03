(set-env!
  :dependencies '[[org.clojure/core.async "0.1.346.0-17112a-alpha"]])

(deftask poly
  "Set env and task options for -poly tasks."
  []
  (set-env!
   :resource-paths #{"src/ion/poly"})
  (task-options!
   pom {:project 'ion/poly
        :version "0.1.0"
        :description "ClojureScript application utilities"
        :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
        :scm {:url "https://github.com/decomplect/ion/src/ion/poly"}
        :url "https://github.com/decomplect/ion"})
  identity)

(deftask build-poly
  "Build and install the ion.poly library."
  []
  (comp (poly)
        (pom)
        (sift :move {#"(^.*\.cljs)$" "ion/poly/$1"})
        (jar)
        (install)))
