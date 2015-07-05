(set-env! :dependencies '[[adzerk/bootlaces "0.1.11" :scope "test"]])

(require '[adzerk.bootlaces :refer :all])

(def +re-source-files+ #"(^.*\.clj[cs]?)$")

(deftask cuss
  "Set env and task options for ion.cuss tasks."
  []
  (let [version "0.1.0-SNAPSHOT"]
    (bootlaces! version)
    (set-env!
     :dependencies '[[garden "1.2.5"]]
     :resource-paths #{"src/ion/cuss"})
    (task-options!
     pom {:project 'ion/cuss
          :version version
          :description "ClojureScript CSS Utilities"
          :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
          :scm {:url "https://github.com/decomplect/ion/src/ion/cuss"}
          :url "https://github.com/decomplect/ion#cuss"}
     sift {:move {+re-source-files+ "ion/cuss/$1"}})
    identity))

(deftask poly
  "Set env and task options for ion.poly tasks."
  []
  (let [version "0.1.0-SNAPSHOT"]
    (bootlaces! version)
    (set-env!
     :dependencies '[[org.clojure/core.async "0.1.346.0-17112a-alpha"]]
     :resource-paths #{"src/ion/poly"})
    (task-options!
     pom {:project 'ion/poly
          :version "0.1.0-SNAPSHOT"
          :description "ClojureScript Application Utilities"
          :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
          :scm {:url "https://github.com/decomplect/ion/src/ion/poly"}
          :url "https://github.com/decomplect/ion#poly"}
     sift {:move {+re-source-files+ "ion/poly/$1"}})
    identity))

(deftask build
  "Build and install a library."
  []
  (comp (pom) (sift) (jar) (install)))
