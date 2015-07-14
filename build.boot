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

(deftask omni
  "Set env and task options for ion.omni tasks."
  []
  (let [version "0.1.0-SNAPSHOT"]
    (bootlaces! version)
    (set-env!
     :dependencies '[[org.clojure/core.async "0.1.346.0-17112a-alpha"]]
     :resource-paths #{"src/ion/omni"})
    (task-options!
     pom {:project 'ion/omni
          :version version
          :description "ClojureScript Event Loop"
          :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
          :scm {:url "https://github.com/decomplect/ion/src/ion/omni"}
          :url "https://github.com/decomplect/ion#omni"}
     sift {:move {+re-source-files+ "ion/omni/$1"}})
    identity))

(deftask poly
  "Set env and task options for ion.poly tasks."
  []
  (let [version "0.1.0-SNAPSHOT"]
    (bootlaces! version)
    (set-env!
     :dependencies '[[org.clojure/core.async "0.1.346.0-17112a-alpha"]
                     [spellhouse/phalanges "0.1.6"
                        :exclusions [com.cemerick/austin
                                     org.clojure/clojure]]]
     :resource-paths #{"src/ion/poly"})
    (task-options!
     pom {:project 'ion/poly
          :version version
          :description "ClojureScript Application Utilities"
          :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}
          :scm {:url "https://github.com/decomplect/ion/src/ion/poly"}
          :url "https://github.com/decomplect/ion#poly"}
     sift {:move {+re-source-files+ "ion/poly/$1"}})
    identity))

(deftask build
  "The pom/sift/jar/install of a library."
  []
  (comp (pom) (sift) (jar) (install)))
