(ns ion.ergo.core
  "Building blocks for the construction of generative systems; stepping stones
   for the pursuit of algorithmic beauty within the gardens of your mind.")

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


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
; Helper Functions

(defn degrees [theta] (* (double theta) DEG))

(defn radians [theta] (* (double theta) RAD))

(defn clamp [min max x]
  (let [x (long x) min (long min) max (long max)]
    (if (< x min) min (if (> x max) max x))))

(defn clamp-normalized [x]
  (let [x (double x)] (if (< x -1.0) -1.0 (if (> x 1.0) 1.0 x))))

(defn neighborhood
  "Returns a function that returns a lazy sequence of [x y] pairs of a
   neighborhood for a given [x y] vector."
  [nf-x nf-y]
  (fn [[x y]]
    (map vector (nf-x x) (nf-y y))))

(def neighborhood-4-x (juxt inc identity dec identity))
(def neighborhood-4-y (juxt identity inc identity dec))

(def neighborhood-4 (neighborhood neighborhood-4-x neighborhood-4-y))

(def neighborhood-5-x (juxt inc identity dec identity identity))
(def neighborhood-5-y (juxt identity inc identity dec identity))

(def neighborhood-5 (neighborhood neighborhood-5-x neighborhood-5-y))

(def neighborhood-8-x (juxt inc inc identity dec dec dec identity inc))
(def neighborhood-8-y (juxt identity inc inc inc identity dec dec dec))

(def neighborhood-8 (neighborhood neighborhood-8-x neighborhood-8-y))

(def neighborhood-9-x (juxt inc inc identity dec dec dec identity inc identity))
(def neighborhood-9-y (juxt identity inc inc inc identity dec dec dec identity))

(def neighborhood-9 (neighborhood neighborhood-9-x neighborhood-9-y))

(defn wi->xy
  "Returns the [x y] coordinates for a row-major order index position based on
   the width of the grid."
  [w i]
  (let [w (long w)
        i (long i)
        x (mod i w)
        y (quot i w)]
    [x y]))

(defn hi->xy
  "Returns the [x y] coordinates for a column-major order index position based
   on the height of the grid."
  [h i]
  (let [h (long h)
        i (long i)
        x (quot i h)
        y (mod i h)]
    [x y]))

(defn row-whxy->i
  "Returns the linear index position in a row-major-ordered 1D array for the
   given [x y] coordinates with toroidal adjustments applied to the x and y
   values based on the grid width and grid height."
  [w h [x y]]
  (let [w (long w)
        h (long h)
        x (long (mod x w))
        y (long (mod y h))
        i (+ x (* y w))]
    i))

(defn col-whxy->i
  "Returns the linear index position in a column-major-ordered 1D array for
   the given [x y] coordinates with toroidal adjustments applied to the x and y
   values based on the grid width and grid height."
  [w h [x y]]
  (let [w (long w)
        h (long h)
        x (long (mod x w))
        y (long (mod y h))
        i (+ (* x h) y)]
    i))


; -----------------------------------------------------------------------------
; Recursive Axiomatic Transformative Sequence (RATS) Producer

(defn produce
  "Returns a lazy sequence of colls from a recursive, axiomatic,
   transformative process."
  [seed prep-f get-xf]
  (letfn [(process
            [coll]
            (lazy-seq
              (when (seq coll)
                (let [new-coll (into (empty coll) (get-xf coll) (prep-f coll))]
                  (cons new-coll (process new-coll))))))]
    (process seed)))

(defn get-rewriting-rules
  "Returns the rules for a generation, calling any rules written as functions."
  [axiom rules generation]
  (if (= 0 generation)
    {::axiom axiom}
    (if (fn? rules) (rules generation) rules)))

(defn rewriting-system
  "Returns a rewriting (Lindenmayer) system."
  [get-xf]
  (produce [::axiom] identity get-xf))

(defn dense-ca-system
  "Returns a densely-populated toroidal grid cellular automata system."
  [seed get-xf]
  (cons seed (produce seed identity get-xf)))

(defn sparse-ca-system
  "Returns a sparsely-populated set-of-cells-based cellular automata system."
  [seed prep-f get-xf]
  (cons (set seed) (produce (set seed) prep-f get-xf)))


; -----------------------------------------------------------------------------
; Transformation Functions

(defprotocol IRewritable
  (module [this]))

(defn modulate
  "Returns the module, or the module supplied by an object implementing the
   Rewrite protocol."
  [m]
  (if (satisfies? IRewritable m) (module m) m))

(defn modulating
  "Returns a module-extracting transducer."
  []
  (map modulate))

(defn rewrite
  "Returns a successor, which must be a vector or a function. If no match is
   found in the rules mapping, the original module is return within a vector."
  [rules m]
  (or (rules m) [m]))

(defn rewriting
  "Returns a rewriting transducer."
  [axiom rules generation]
  (map (partial rewrite (get-rewriting-rules axiom rules generation))))

(defn call
  "Returns a function that will return the successor vector, or the result of
   calling the successor function, which must return a vector of modules."
  ([]
   (fn context-free-call [successor]
     (if (fn? successor)
       (successor)
       successor)))
  ([generation word]
   (let [index (volatile! (long -1))]
     (fn context-sensitive-call [successor]
       (vswap! index #(inc (long %)))
       (if (fn? successor)
         (successor generation word @index (word @index))
         successor)))))

(defn calling
  "Returns a function-calling transducer."
  ([]
   (map (call)))
  ([generation word]
   (map (call generation word))))

(defn cell-life
  "Returns a function that returns a cell whose fate depends on the number of
   live neighboring cells."
  [live-cell dead-cell survive? birth? live-count-f neighbors-f]
  (let [index (volatile! (long -1))]
    (fn cell-fate [cell]
      (vswap! index #(inc (long %)))
      (let [neighbors (neighbors-f @index)
            live-neighbor-count (reduce live-count-f 0 neighbors)]
        (if (= live-cell cell)
          (if (survive? live-neighbor-count) live-cell dead-cell)
          (if (birth? live-neighbor-count) live-cell dead-cell))))))

(defn cell-life-candidate-set
  "Returns a function that returns a cell whose fate depends on the number of
   live neighboring cells."
  [live-cell dead-cell survive? birth? live-count-f neighbors-f candidate?]
  (let [index (volatile! (long -1))]
    (fn cell-fate [cell]
      (vswap! index #(inc (long %)))
      (if-not (candidate? @index)
        dead-cell
        (let [neighbors (neighbors-f @index)
              live-neighbor-count (reduce live-count-f 0 neighbors)]
          (if (= live-cell cell)
            (if (survive? live-neighbor-count) live-cell dead-cell)
            (if (birth? live-neighbor-count) live-cell dead-cell)))))))

(defn cell-life-candidate-counts
  "Returns a function that returns a cell whose fate depends on the number of
   live neighboring cells."
  [live-cell dead-cell survive? birth? candidate-counts]
  (let [index (volatile! (long -1))]
    (fn cell-fate [cell]
      (vswap! index #(inc (long %)))
      (if-let [live-neighbor-count (candidate-counts @index)]
        (if (= live-cell cell)
          (if (survive? live-neighbor-count) live-cell dead-cell)
          (if (birth? live-neighbor-count) live-cell dead-cell))
        dead-cell))))


; -----------------------------------------------------------------------------
; Generation Number and Data Context Wrapper

(defn gen
  "Returns a function that, when called, will call f with an incremented
   generation number and an additional context data argument."
  [f]
  (let [generation (volatile! (long -1))]
    (fn
      [data]
      (vswap! generation #(inc (long %)))
      (f @generation data))))


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
  (rewriting-system (gen (fn [generation _]
                           (comp (rewriting axiom rules generation)
                                 cat)))))

(defn functional-rewriting-system
  "Returns a lazy sequence of words from a context-free rewriting process.
   Allows a rewrite successor to (optionally) be a function that will get
   called without passing it any arguments. The returned value may be
   deterministic or stochastic."
  [axiom rules]
  (rewriting-system (gen (fn [generation _]
                           (comp (rewriting axiom rules generation)
                                 (calling)
                                 cat)))))

(defn context-sensitive-rewriting-system
  "Returns a lazy sequence of words from a context-sensitive rewriting
   process. Allows a rewrite successor to (optionally) be a function that will
   get called with the current context (generation word index module) as args."
  [axiom rules]
  (rewriting-system (gen (fn [generation word]
                           (comp (rewriting axiom rules generation)
                                 (calling generation word)
                                 cat)))))

(defn parametric-rewriting-system
  "Returns a lazy sequence of words from a context-free rewriting process."
  [axiom rules]
  (rewriting-system (gen (fn [generation _]
                           (comp (modulating)
                                 (rewriting axiom rules generation)
                                 cat)))))

(defn parametric-functional-rewriting-system
  "Returns a lazy sequence of words from a context-free rewriting process.
   Allows a rewrite successor to (optionally) be a function that will get
   called without passing it any arguments. The returned value may be
   deterministic or stochastic."
  [axiom rules]
  (rewriting-system (gen (fn [generation _]
                           (comp (modulating)
                                 (rewriting axiom rules generation)
                                 (calling)
                                 cat)))))

(defn parametric-context-sensitive-rewriting-system
  "Returns a lazy sequence of words from a context-sensitive rewriting
   process. Allows a rewrite successor to (optionally) be a function that will
   get called with the current context (generation word index module) as args."
  [axiom rules]
  (rewriting-system (gen (fn [generation word]
                           (comp (modulating)
                                 (rewriting axiom rules generation)
                                 (calling generation word)
                                 cat)))))

(comment ; A basic example.

  (defn basic-fibonacci-sequence
    "Returns a lazy sequence of vectors of Fibonacci integers - OEIS A003849."
    []
    (apply basic-rewriting-system (grammar :A003849)))

  (-> (basic-fibonacci-sequence) (nth 10) count) ; 144

  )

; -----------------------------------------------------------------------------
; General Cellular Automata

(declare patternpedia)

(defn pattern
  "Returns the set of cells specified by the key from the patternpedia."
  [k]
  (k patternpedia))


; -----------------------------------------------------------------------------
; Life-Like Cellular Automata

(declare lifepedia)

(defn life-rules
  "Returns the [survive? birth? neighborhood] rules trifecta from lifepedia."
  [k]
  (let [info (k lifepedia)]
    [(:S info) (:B info) (:N info)]))


; -----------------------------------------------------------------------------
; Densely-Populated Toroidal Grids of Cellular Automata

(defn make-seed
  "Returns a vector of values based on calling f, which should return a lazy
   infinite sequence."
  [f w h]
  (vec (take (* (long w) (long h)) (f))))

(defn make-seed-with-random-values
  [cell-values w h]
  (make-seed #(repeatedly (fn [] (rand-nth cell-values))) w h))

(defn make-seed-2
  "Returns a vector of values based on calling (f x y)."
  [w h f]
  (vec (for [y (range h)
             x (range w)]
         (f x y))))

(defn make-seed-for-pattern
  [live-cell dead-cell w h pattern]
  (let [make-cell (fn [x y]
                    (if (pattern [x y]) live-cell dead-cell))]
    (make-seed-2 w h make-cell)))

(defn make-seed-for-acorn
  [live-cell dead-cell w h]
  (make-seed-for-pattern
    live-cell dead-cell w h #{[0 2] [1 0] [1 2] [3 1] [4 2] [5 2] [6 2]}))

(defn cell-counter
  "Returns a functions that counts cells having a certain value."
  [cell-value]
  (fn [n cell] (if (= cell-value cell) (inc (long n)) n)))

(defn make-neighborhood-lookup
  "Returns a row-major order vector of vectors of neighborhood positions for
   each cell position for a given width and height."
  [neighborhood-f width height]
  (let [xy->i (partial row-whxy->i width height)]
    (into [] (for [y (range height)
                   x (range width)]
               (mapv xy->i (neighborhood-f [x y]))))))

(defn get-neighbors
  "Returns a function that returns a lazy sequence of neighbors of the cell at
   index in word."
  [neighborhood-lookup word]
  (fn [index] (map word (neighborhood-lookup index))))

(defn get-candidates
  "Returns the set of cell-index values for non-dead cells and their
   neighbors."
  [dead-cell neighborhood-lookup word]
  (let [f (fn [index cell]
            (when (not= dead-cell cell)
              (conj (neighborhood-lookup index) index)))]
    (set (apply concat (keep-indexed f word)))))

(defn get-candidate-counts
  "Returns a map of candidate-cell-index neighbor-count pairs."
  [dead-cell neighborhood-lookup word]
  (let [f (fn [index cell]
            (when (not= dead-cell cell)
              (neighborhood-lookup index)))]
    (frequencies (apply concat (keep-indexed f word)))))

(defn dense-fate-ca-system
  [cell-fate-f neighborhood-f seed w h]
  (let [neighborhood-lookup (make-neighborhood-lookup neighborhood-f w h)]
    (dense-ca-system
      seed
      (gen (fn [generation word]
             (let [neighbors-f (get-neighbors neighborhood-lookup word)]
               (map (cell-fate-f generation neighbors-f))))))))

(defn dense-life-ca-system
  [survive? birth? neighborhood-f live-cell dead-cell seed w h]
  (let [neighborhood-lookup (make-neighborhood-lookup neighborhood-f w h)
        live-count-f (cell-counter live-cell)
        life (partial cell-life live-cell dead-cell
                      survive? birth? live-count-f)]
    (dense-ca-system
      seed
      (gen (fn [generation word]
             (let [neighbors-f (get-neighbors neighborhood-lookup word)]
               (map (life neighbors-f))))))))

(defn dense-life-candidate-set-ca-system
  [survive? birth? neighborhood-f live-cell dead-cell seed w h]
  (let [neighborhood-lookup (make-neighborhood-lookup neighborhood-f w h)
        live-count-f (cell-counter live-cell)
        life (partial cell-life-candidate-set live-cell dead-cell
                      survive? birth? live-count-f)]
    (dense-ca-system
      seed
      (gen (fn [generation word]
             (let [neighbors-f (get-neighbors neighborhood-lookup word)
                   candidate? (get-candidates dead-cell neighborhood-lookup word)]
               (map (life neighbors-f candidate?))))))))

(defn dense-life-candidate-counts-ca-system
  [survive? birth? neighborhood-f live-cell dead-cell seed w h]
  (let [neighborhood-lookup (make-neighborhood-lookup neighborhood-f w h)
        life (partial cell-life-candidate-counts live-cell dead-cell
                      survive? birth?)]
    (dense-ca-system
      seed
      (gen (fn [generation word]
             (let [counts (get-candidate-counts dead-cell neighborhood-lookup word)]
               (map (life counts))))))))

(defn dense-life-rule-system
  [rule-key live-cell dead-cell seed w h]
  (let [[survive? birth? neighborhood-f] (life-rules rule-key)]
    (dense-life-ca-system
      survive? birth? neighborhood-f live-cell dead-cell seed w h)))

(comment

  (defn dense-random-conway-game-of-life
    "Example of Conway's Game of Life in a densely-populated toroidal grid,
     representing live cells as :alive and dead cells as :dead, randomly
     seeded."
    [w h]
    (dense-life-ca-system
      #{2 3} #{3} neighborhood-8 :alive :dead
      (make-seed-for-random-value [:alive :dead] w h) w h))

  )


; -----------------------------------------------------------------------------
; Protocols/Records/Types for Sparse Sets of Cellular Automata

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
  ([[x y]] ; Candidate cell, only used temporarily to test set membership.
   (->Cell x y))
  ([[x y] cells] ; Newborn cell.
   (->Cell x y))
  ([[x y] cells cell] ; Survivor.
   (->Cell x y)))

(defn vector-cell
  ([v] ; Candidate cell, only used temporarily to test set membership.
   v)
  ([v cells] ; Newborn cell.
   v)
  ([v cells cell] ; Survivor.
   v))


; -----------------------------------------------------------------------------
; Sparse Sets of Life-Like Cellular Automata

(defn neighbor-frequencies
  "Returns a map of [x y] neighbor-count pairs for a set of cells based on the
   neighborhood function."
  [neighborhood-f cells]
  (frequencies (mapcat #(neighborhood-f (position %)) cells)))

(defn sparse-life-exist
  "Returns a function that returns a cell if destiny will allow it to survive,
   or a newborn cell if mother nature brings it to life."
  [survive? birth? cell-maker-f cells]
  (fn sparse-life-cell-fate [[cell-position neighbor-count]]
    (if-let [cell (cells (cell-maker-f cell-position))]
      (when (survive? neighbor-count) (cell-maker-f cell-position cells cell))
      (when (birth? neighbor-count) (cell-maker-f cell-position cells)))))

(defn sparse-life-existing
  "Returns an existence-determining transducer."
  [survive? birth? cell-maker-f cells]
  (keep (sparse-life-exist survive? birth? cell-maker-f cells)))

(defn sparse-life-xf
  [survive? birth? cell-maker-f cells]
  ; TODO comp in a trim function based on the x & y extents.
  (sparse-life-existing survive? birth? cell-maker-f cells))

(defn sparse-life-ca-system
  [survive? birth? neighborhood-f cell-maker-f seed]
  (let [seed (into #{} (map cell-maker-f) seed)
        prep-f (partial neighbor-frequencies neighborhood-f)
        get-xf (partial sparse-life-xf survive? birth? cell-maker-f)]
    (sparse-ca-system seed prep-f get-xf)))

(defn sparse-life-rule-system
  [rule-key cell-maker-f seed]
  (let [[survive? birth? neighborhood-f] (life-rules rule-key)]
    (sparse-life-ca-system survive? birth? neighborhood-f cell-maker-f seed)))

(comment

  "Conway's Game of Life example using Acorn pattern as the seed."

  (->
    (sparse-life-rule-system :conway-game-of-life vector-cell (pattern :acorn))
    (nth 10))

  (->
    (sparse-life-rule-system :conway-game-of-life basic-cell (pattern :acorn))
    (nth 10))

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


; -----------------------------------------------------------------------------
; Life-Like Cellular Automata Rules

(def lifepedia
  {:conway-game-of-life
   {:S #{2 3} :B #{3} :N neighborhood-8}
   :fredkin
   {:S #{1 3 5 7 9} :B #{1 3 5 7 9} :N neighborhood-9}
   :gnarl
   {:S #{1} :B #{1} :N neighborhood-8}
   :replicator
   {:S #{1 3 5 7} :B #{1 3 5 7} :N neighborhood-8}
   :seeds
   {:S #{} :B #{2} :N neighborhood-8}
   })


; -----------------------------------------------------------------------------
; Patterns for Cellular Automata

(def patternpedia
  {:acorn
   #{[0 2] [1 0] [1 2] [3 1] [4 2] [5 2] [6 2]}
   :blinker
   #{[1 0] [1 1] [1 2]}
   :glider
   #{[1 0] [2 1] [0 2] [1 2] [2 2]}
   :square
   #{[1 0] [0 1] [1 1] [0 0]}
   })
