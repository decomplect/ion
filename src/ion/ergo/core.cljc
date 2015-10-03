(ns ion.ergo.core
  "A toolkit for the construction of generative systems.")

(set! *warn-on-reflection* true)
;(set! *unchecked-math* :warn-on-boxed)


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

(defprotocol IRewritable
  (module [this]))

(defprotocol IPosition
  (position [this])
  (x [this])
  (y [this])
  (z [this]))

(extend-protocol IPosition
  clojure.lang.PersistentVector
  (position [v] v)
  (x [v] (get v 0))
  (y [v] (get v 1))
  (z [v] (get v 2)))

#?(:clj
   (deftype Cell [x-coord y-coord]
     IPosition
     (position [_] [x-coord y-coord])
     (x [_] x-coord)
     (y [_] y-coord)
     Object
     (equals [_ o]
       (and (satisfies? IPosition o)
            (and (= x-coord (x ^Cell o))
                 (= y-coord (y ^Cell o)))))
     (hashCode [_] (hash [x-coord y-coord]))))

#?(:clj
   (defmethod clojure.core/print-method Cell [this ^java.io.Writer writer]
     (.write writer (str "<Cell " (position this) ">"))))

#?(:cljs
   (deftype Cell [x-coord y-coord]
     IPosition
     (position [_] [x-coord y-coord])
     (x [_] x-coord)
     (y [_] y-coord)
     IEquiv
     (-equiv [_ o]
       (and (instance? Cell o)
            (and (= x-coord (x ^Cell o))
                 (= y-coord (y ^Cell o)))))
     IHash
     (-hash [_] (hash [x-coord y-coord]))))

(defn basic-cell
  ([[x y]]
   (->Cell x y))
  ([[x y] cells]
   (->Cell x y))
  ([[x y] cells cell]
   (->Cell x y)))

(defn vector-cell
  ([v] ; Candidate cell, only used temporarily to test set membership.
   v)
  ([v cells] ; Newborn cell.
   v)
  ([v cells cell] ; Survivor.
   v))


; -----------------------------------------------------------------------------
; Helper Functions

(defn degrees [theta] (* (double theta) DEG))

(defn radians [theta] (* (double theta) RAD))

(defn clamp [min max x]
  (let [x (long x) min (long min) max (long max)]
    (if (< x min) min (if (> x max) max x))))

(defn clamp-normalized [x]
  (let [x (double x)] (if (< x -1.0) -1.0 (if (> x 1.0) 1.0 x))))

(def neighborhood-4-x (juxt inc identity dec identity))
(def neighborhood-4-y (juxt identity inc identity dec))

(defn neighborhood-4 [[x y]] ; von Neumann neighborhood.
  (map vector (neighborhood-4-x x) (neighborhood-4-y y)))

(def neighborhood-5-x (juxt inc identity dec identity identity))
(def neighborhood-5-y (juxt identity inc identity dec identity))

(defn neighborhood-5 [[x y]]
  (map vector (neighborhood-5-x x) (neighborhood-5-y y)))

(def neighborhood-8-x (juxt inc inc identity dec dec dec identity inc))
(def neighborhood-8-y (juxt identity inc inc inc identity dec dec dec))

(defn neighborhood-8 [[x y]] ; Moore neighborhood.
  (map vector (neighborhood-8-x x) (neighborhood-8-y y)))

(def neighborhood-9-x (juxt inc inc identity dec dec dec identity inc identity))
(def neighborhood-9-y (juxt identity inc inc inc identity dec dec dec identity))

(defn neighborhood-9 [[x y]]
  (map vector (neighborhood-9-x x) (neighborhood-9-y y)))

(defn neighbor-freq-4
  "Returns a map of [x y] neighbor-count pairs."
  [cells]
  (frequencies (mapcat #(neighborhood-4 (position %)) cells)))

(defn neighbor-freq-5
  "Returns a map of [x y] neighbor-count pairs."
  [cells]
  (frequencies (mapcat #(neighborhood-5 (position %)) cells)))

(defn neighbor-freq-8
  "Returns a map of [x y] neighbor-count pairs."
  [cells]
  (frequencies (mapcat #(neighborhood-8 (position %)) cells)))

(defn neighbor-freq-9
  "Returns a map of [x y] neighbor-count pairs."
  [cells]
  (frequencies (mapcat #(neighborhood-9 (position %)) cells)))


; -----------------------------------------------------------------------------
; Recursive Axiomatic Transducible Sequence (RATS) Producer

(defn produce
  "Returns a lazy sequence of colls from a recursive, axiomatic, transducible
   process."
  [seed prep-f get-xf]
  (letfn [(process
            [coll]
            (lazy-seq
              (when (seq coll)
                (let [new-coll (into (empty coll) (get-xf coll) (prep-f coll))]
                  (cons new-coll (process new-coll))))))]
    (process seed)))

(defn ca-system
  [seed prep-f get-xf]
  (cons (set seed) (produce (set seed) prep-f get-xf)))

(def rewriting-system (partial produce [::axiom] identity))

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
  (if (satisfies? IRewritable m) (module m) m))

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
   (let [index (volatile! (long -1))]
     (fn context-sensitive-call [successor]
       (vswap! index #(inc (long %)))
       (if (fn? successor)
         (successor g w @index (get w @index))
         successor)))))

(defn exist
  "Returns a cell if destiny will allow, or mother nature brings it to life."
  [survive? birth? cell-maker-f cells [cell-position neighbor-count]]
  (if-let [cell (cells (cell-maker-f cell-position))]
    (when (survive? neighbor-count) (cell-maker-f cell-position cells cell))
    (when (birth? neighbor-count) (cell-maker-f cell-position cells))))


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

(defn existing
  "Returns an existence-determining transducer."
  [survive? birth? cell-maker-f cells]
  (keep (partial exist survive? birth? cell-maker-f cells)))


; -----------------------------------------------------------------------------
; Generation Number and Data Context Wrapper

(defn gen
  "Returns a function that, when called, will call f with an incremented
   generation number and an additional context argument."
  [f]
  (let [generation (volatile! (long -1))]
    (fn
      [data]
      (vswap! generation #(inc (long %)))
      (f @generation data))))


; -----------------------------------------------------------------------------
; Cellular Automata

(declare cellupedia)

(defn ca-rules
  "Returns the [survive? birth? neighbor-freq] rules trifecta from cellupedia."
  [key]
  (let [info (key cellupedia)]
    [(:S info) (:B info) (:N info)]))

(declare patternpedia)

(defn pattern
  "Returns the set of cells specified by the key from the patternpedia."
  [key]
  (key patternpedia))

(defn ca-xf
  [survive? birth? cell-maker-f cells]
  ; TODO comp in a trim function based on the grid size/behavior rules.
  (existing survive? birth? cell-maker-f cells))

(defn ca-sequence
  [survive? birth? neighbor-freq-f cell-maker-f seed]
  (let [seed (into #{} (map cell-maker-f) seed)
        prep-f neighbor-freq-f
        get-xf (partial ca-xf survive? birth? cell-maker-f)]
    (ca-system seed prep-f get-xf)))

(defn ca-builder
  [rule-key cell-maker-f seed]
  (let [[survive? birth? neighbor-freq-f] (ca-rules rule-key)]
    (ca-sequence survive? birth? neighbor-freq-f cell-maker-f seed)))

(comment ; Conway's Game of Life example using Acorn pattern as the seed.

  (-> (ca-builder :conway-game-of-life vector-cell (pattern :acorn)) (nth 10))

  (-> (ca-builder :conway-game-of-life basic-cell (pattern :acorn)) (nth 10))

  )


; -----------------------------------------------------------------------------
; Patternpedia

(def patternpedia
  {:acorn
   #{[70 62] [71 60] [71 62] [73 61] [74 62] [75 62] [76 62]}
   })


; -----------------------------------------------------------------------------
; Cellupedia

(def cellupedia
  {:conway-game-of-life
   {:S #{2 3} :B #{3} :N neighbor-freq-8}
   :gnarl
   {:S #{1} :B #{1} :N neighbor-freq-8}
   :replicator
   {:S #{1 3 5 7} :B #{1 3 5 7} :N neighbor-freq-8}
   :fredkin
   {:S #{1 3 5 7 9} :B #{1 3 5 7 9} :N neighbor-freq-9}
   })


; -----------------------------------------------------------------------------
; Rewriting Systems

(declare grammarpedia)

(defn grammar
  "Returns the [axiom rules] vector from the grammarpedia."
  [key]
  (let [gramm (key grammarpedia)]
    [(:axiom gramm) (:rules gramm)]))

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

(comment ; A basic example.

  (defn basic-fibonacci-sequence
    "Returns a lazy sequence of vectors of Fibonacci integers - OEIS A003849."
    []
    (apply basic-rewriting-system (grammar :A003849)))

  (-> (basic-fibonacci-sequence) (nth 10) count) ; 144

  )

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
