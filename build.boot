(set-env!
  :dependencies '[[org.clojure/clojurescript "0.0-3308"]
                  [org.clojure/core.async "0.1.346.0-17112a-alpha"]])

(deftask poly []
  (set-env!
   :resource-paths #{"src/ion/poly"})
  (task-options!
   pom {:project 'ion/poly
        :version "0.1.0"
        :description "ClojureScript application utilities"
        :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
        :scm {:url "https://github.com/decomplect/ion/src/ion/poly"}
        :url "https://github.com/decomplect/ion"}))

(deftask build-poly []
  (comp (poly)
        (pom)
        (jar)
        (install)))
