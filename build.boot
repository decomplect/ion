(set-env!
  :dependencies '[[org.clojure/core.async "0.1.346.0-17112a-alpha"]])

(deftask cuss
  "Set env and task options for -cuss tasks."
  []
  (set-env!
   :resource-paths #{"src/ion/cuss"})
  (task-options!
   pom {:project 'ion/cuss
        :version "0.1.0-SNAPSHOT"
        :description "ClojureScript CSS Utilities"
        :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
        :scm {:url "https://github.com/decomplect/ion/src/ion/cuss"}
        :url "https://github.com/decomplect/ion"}
   sift {:move {#"(^.*\.cljs)$" "ion/poly/$1"}})
  identity)

(deftask poly
  "Set env and task options for -poly tasks."
  []
  (set-env!
   :resource-paths #{"src/ion/poly"})
  (task-options!
   pom {:project 'ion/poly
        :version "0.1.0-SNAPSHOT"
        :description "ClojureScript Application Utilities"
        :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
        :scm {:url "https://github.com/decomplect/ion/src/ion/poly"}
        :url "https://github.com/decomplect/ion"}
   sift {:move {#"(^.*\.cljs)$" "ion/poly/$1"}})
  identity)

(deftask build
  "Build and install a library."
  []
  (comp (pom)
        (sift)
        (jar)
        (install)))
