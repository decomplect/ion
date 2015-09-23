(ns ion.ergo.core
  "A toolkit for the construction of generative systems.")


; -----------------------------------------------------------------------------
; Shared Constants

(def ^:const PI Math/PI)

(def ^:const THREE-HALVES-PI (* PI 1.5))
(def ^:const TWO-PI (* PI 2.0))

(def ^:const HALF-PI (/ PI 2.0))
(def ^:const THIRD-PI (/ PI 3.0))
(def ^:const QUARTER-PI (/ PI 4.0))
(def ^:const SIXTH-PI (/ PI 6.0))

(def ^:const DEG (/ 180.0 PI))
(def ^:const RAD (/ PI 180.0))


; -----------------------------------------------------------------------------
; Protocols / Records / Types

(defprotocol Coordinates (point [this]))

(defprotocol Rewrite (module [this]))

#?(:cljs
   (deftype Cell [x y]
     Coordinates
     (point [_] [x y])
     ICounted
     (-count [_] 2)
     IIndexed
     (-nth [_ i]
       (case i
         0 x
         1 y))
     (-nth [_ i not-found]
       (case i
         0 x
         1 y
         not-found))
     ILookup
     (-lookup [this k] (-lookup this k nil))
     (-lookup [_ k _]
       (case k
         0 x
         1 y))
     ISeqable
     (-seq [_] (seq [x y])))
   :clj
   (deftype Cell [x y]
     Coordinates
     (point [_] [x y])
     clojure.lang.Counted
     (count [_] 2)
     clojure.lang.Indexed
     (nth [_ i]
       (case i
         0 x
         1 y
         (throw (IllegalArgumentException.))))
     (nth [this i _] (nth this i))
     clojure.lang.ILookup
     (valAt [this k] (.valAt this k nil))
     (valAt [_ k _]
       (case k
         0 x
         1 y
         (throw (IllegalArgumentException.))))
     clojure.lang.Seqable
     (seq [_] (seq [x y]))))


; -----------------------------------------------------------------------------
; Helper Functions

(declare grammarpedia)

(defn grammar
  [key]
  (let [gramm (key grammarpedia)]
    [(:axiom gramm) (:rules gramm)]))

(defn degrees [theta] (* theta DEG))

(defn radians [theta] (* theta RAD))

(defn clamp [x min max]
  (if (< x min) min (if (> x max) max x)))

(defn clamp-normalized [x]
  (if (< x -1.0) -1.0 (if (> x 1.0) 1.0 x)))

(defn neighborhood-4 [[x y]]
  (map vector
       ((juxt inc identity dec identity) x)
       ((juxt identity inc identity dec) y)))

(defn neighborhood-5 [[x y]]
  (map vector
       ((juxt inc identity dec identity identity) x)
       ((juxt identity inc identity dec identity) y)))

(def neighborhood-8-x (juxt inc inc identity dec dec dec identity inc))
(def neighborhood-8-y (juxt identity inc inc inc identity dec dec dec))

(defn neighborhood-8 [[x y]]
  (map ->Cell (neighborhood-8-x x) (neighborhood-8-y y)))

(def neighborhood-9-x (juxt inc inc identity dec dec dec identity inc identity))
(def neighborhood-9-y (juxt identity inc inc inc identity dec dec dec identity))

(defn neighborhood-9 [[x y]]
  (map ->Cell (neighborhood-9-x x) (neighborhood-9-y y)))

(defn neighbor-freq-8
  "Returns a map of cell, neighbor-count pairs."
  [cells]
  (frequencies (mapcat neighborhood-8 cells)))

(defn neighbor-freq-9
  "Returns a map of cell, neighbor-count pairs."
  [cells]
  (frequencies (mapcat neighborhood-9 cells)))


; -----------------------------------------------------------------------------
; Recursive Axiomatic Transducible Sequence (RATS) Producer

(defn produce
  "Returns a lazy sequence of colls from a recursive, axiomatic, transducible
   process."
  [init-f prep-f seed-f get-xf]
  (letfn [(process
            [coll]
            (lazy-seq
              (when (seq coll)
                (let [new-coll (into (init-f) (get-xf coll) (prep-f coll))]
                  (cons new-coll (process new-coll))))))]
    (process (into (init-f) (seed-f)))))

(defn ca-system
  [prep seed get-xf]
  (cons (set seed) (produce #(set nil) prep #(set seed) get-xf)))

(def rewriting-system (partial produce #(vec nil) identity #(vec [::axiom])))

(defn get-rewriting-rules
  "Returns the rules for a generation, calling any rules written as functions."
  [axiom rules generation]
  (if (= 0 generation)
    {::axiom axiom}
    (if (fn? rules) (rules generation) rules)))


; -----------------------------------------------------------------------------
; Transformation Functions

(defn modulate
  "Returns the module or the module supplied by an object implementing the
   Rewrite protocol."
  [m]
  (if (satisfies? Rewrite m) (module m) m))

(defn rewrite
  "Returns a successor, which must be a vector or a function. If no match is
   found in the rules mapping, the original module is return within a vector."
  [rules m]
  (or (rules m) [m]))

(defn call
  "Returns a function that will return the successor vector, or the result of
   calling the successor function, which must return a vector of modules."
  ([]
   (fn context-free-call [successor]
     (if (fn? successor)
       (successor)
       successor)))
  ([g w]
   (let [index (volatile! -1)]
     (fn context-sensitive-call [successor]
       (vswap! index inc)
       (if (fn? successor)
         (successor g w @index (get w @index))
         successor)))))

(defn exist?
  "Returns a cell if destiny will allow, or mother nature brings it to life."
  [survive? birth? cells [cell neighbor-count]]
  (if (cells cell)
    (when (survive? neighbor-count) cell)
    (when (birth? neighbor-count) cell)))


; -----------------------------------------------------------------------------
; Transducers

(defn modulating
  "Returns a module-extracting transducer."
  []
  (map modulate))

(defn rewriting
  "Returns a rewriting transducer."
  [axiom rules generation]
  (map (partial rewrite (get-rewriting-rules axiom rules generation))))

(defn calling
  "Returns a function-calling transducer."
  ([]
   (map (call)))
  ([g w]
   (map (call g w))))

(defn existing?
  "Returns an existence-determining transducer."
  [survive? birth? cells]
  (keep (partial exist? survive? birth? cells)))


; -----------------------------------------------------------------------------
; Generation Number and Data Context Wrapper

(defn gen
  "Returns a function that, when called, will call f with an incremented
   generation number and an additional context argument."
  [f]
  (let [generation (volatile! -1)]
    (fn
      [data]
      (vswap! generation inc)
      (f @generation data))))


; -----------------------------------------------------------------------------
; Cellular Automata

(declare cellupedia)

(defn ca-rules
  "Returns the [survive? birth? neighbor-freq] rules trifecta from cellupedia."
  [key]
  (let [info (key cellupedia)]
    [(:S info) (:B info) (:N info)]))

(defn ca-xf
  [survive? birth? cells]
  ; TODO comp in a trim function based on the grid size/behavior rules.
  (existing? survive? birth? cells))

(defn ca-sequence
  [survive? birth? neighbor-freq seed]
  (ca-system neighbor-freq seed (partial ca-xf survive? birth?)))

(defn ca-builder
  [key]
  (partial (apply ca-sequence (ca-rules key))))

(def ca-conway (ca-builder :conway))

(def ca-gnarl (ca-builder :gnarl))


; -----------------------------------------------------------------------------
; Cellupedia

(def cellupedia
  {:conway
   {:S #{2 3}
    :B #{3}
    :N neighbor-freq-8
    }
   :gnarl
   {:S #{1}
    :B #{1}
    :N neighbor-freq-8
    }
   })


; -----------------------------------------------------------------------------
; Rewriting Systems

(defn basic-rewriting-system
  "Returns a lazy sequence of words from a context-free rewriting process."
  [axiom rules]
  (rewriting-system (gen (fn [g _]
                           (comp (rewriting axiom rules g)
                                 cat)))))

(defn functional-rewriting-system
  "Returns a lazy sequence of words from a context-free rewriting process.
   Allows a rewrite successor to (optionally) be a function that will get
   called without passing it any arguments. The returned value may be
   deterministic or stochastic."
  [axiom rules]
  (rewriting-system (gen (fn [g _]
                           (comp (rewriting axiom rules g)
                                 (calling)
                                 cat)))))

(defn context-sensitive-rewriting-system
  "Returns a lazy sequence of words from a context-sensitive rewriting
   process. Allows a rewrite successor to (optionally) be a function that will
   get called with the current context (g w i m) as arguments."
  [axiom rules]
  (rewriting-system (gen (fn [g w]
                           (comp (rewriting axiom rules g)
                                 (calling g w)
                                 cat)))))

(defn parametric-rewriting-system
  "Returns a lazy sequence of words from a context-free rewriting process."
  [axiom rules]
  (rewriting-system (gen (fn [g _]
                           (comp (modulating)
                                 (rewriting axiom rules g)
                                 cat)))))

(defn parametric-functional-rewriting-system
  "Returns a lazy sequence of words from a context-free rewriting process.
   Allows a rewrite successor to (optionally) be a function that will get
   called without passing it any arguments. The returned value may be
   deterministic or stochastic."
  [axiom rules]
  (rewriting-system (gen (fn [g _]
                           (comp (modulating)
                                 (rewriting axiom rules g)
                                 (calling)
                                 cat)))))

(defn parametric-context-sensitive-rewriting-system
  "Returns a lazy sequence of words from a context-sensitive rewriting
   process. Allows a rewrite successor to (optionally) be a function that will
   get called with the current context (g w i m) as arguments."
  [axiom rules]
  (rewriting-system (gen (fn [g w]
                           (comp (modulating)
                                 (rewriting axiom rules g)
                                 (calling g w)
                                 cat)))))


; -----------------------------------------------------------------------------
; Grammarpedia

; When a grammar produces an integer sequence, it is named after its identifier
; from "The On-Line Encyclopedia of Integer Sequences" https://oeis.org/

(def grammarpedia
  {:A003849
   {:descr "Fibonacci sequence, beginning with zero"
    :axiom [0]
    :rules {0 [0 1]
            1 [0]}}
   :A005614
   {:descr "Fibonacci sequence, beginning with one"
    :axiom [1]
    :rules {0 [1]
            1 [1 0]}}
   :A010060
   {:descr "Thue-Morse sequence"
    :axiom [0]
    :rules {0 [0 1]
            1 [1 0]}}
   :A014577
   {:descr "Dragon-curve sequence"
    :axiom [:L]
    :rules {:L [:L :1 :R]
            :R [:L :0 :R]}}
   :A026465
   {:descr "Length of n-th run of identical symbols in the Thue-Morse sequence A010060"
    :axiom [1]
    :rules {1 [1 2 1]
            2 [1 2 2 2 1]}}
   :A029883
   {:descr "First differences of Thue-Morse sequence A001285"
    :axiom [1]
    :rules {1 [1 0 -1]
            0 [1 -1]
            -1 [0]}}
   :A036577
   {:descr "Ternary Thue-Morse sequence"
    :axiom [2]
    :rules {0 [1]
            1 [2 0]
            2 [2 1 0]}}
   :A166253
   {:descr "A166253"
    :axiom [1]
    :rules {0 [0 1 1 1 0]
            1 [1 0 0 0 1]}}
   :Rudin-Shapiro-sequence
   {:descr "Rudin-Shapiro sequence"
    :axiom [:AA]
    :rules {:AA [:AA :AB]
            :AB [:AA :BA]
            :BA [:BB :AB]
            :BB [:BB :BA]}}
   :binary
   {:descr "Binary sequence"
    :axiom [0]
    :rules {0 [0 1 0]}}
   })
