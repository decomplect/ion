(ns ion.ergo.l-system
  "Support for Lindenmayer systems: deterministic, stochastic, context-free,
   context-sensitive, or parametric. Or combinations thereof.

   A somewhat confusing variety of terms are used to describe L-systems and
   their component parts, and this source code file is likely no exception. In
   order to be understood in the context of existing semiotics we have adhered
   as closely as possible to the most commonly used terminology. In particular,
   however, we avoid using the term \"String\" so as to avoid confusion with
   the datatype of the same name. Instead we refer to that part of the system
   as a \"Word\", although it should not be confused with the everyday notion
   of a word in a spoken language. And while we avoid calling a Word a String,
   a Word is typically represented in an L-system by a `string`, although it
   doesn't need to be. And a Word is a sequence of somethings. For similar
   reasons we prefer to call those \"somethings\" Modules instead of the other
   commonly used term \"Letters\". (And so begins the inevitable confusion...)

   With that in mind, we describe an L-system as a parallel rewriting system.
   A rewriting system takes an intial input value made up of a collection of
   one or more symbols (we call that collection of symbols a \"Word\"), and
   recursively produces new \"Words\" by replacing each input symbol (called a
   Module) with itself or a successor symbol/module (or collection of modules)
   determined according to a set of production rules. The rewriting is treated
   as if the replacements all took place in parallel.

   L-systems operate on words, which are sequences of modules. When
   represented programatically, modules can be simple keywords or characters,
   or they can be compound structures containing additional parameters or local
   state.

   Beginning with an intial word, called an axiom, an L-system generates a
   developmental sequence of words by recusively applying a set of productions,
   or replacement rules. The resulting seqence of words can then be used as
   data for futher processing, such as to be rendered graphically or output as
   music.

   Because productions tend to replace one module with more than one module,
   words tend to grow in size. And because this growth is recursive, L-systems
   are useful for modeling a variety of processes such as: fractal geometry,
   the growth of plants, morphogenesis, crystallography, and more.")

(defn split-successor
  "Returns a sequence of modules, updating new-state with any successor data."
  [successor new-state index]
  (vec ; We don't want this to be lazy since it has side-effects.
    (for [[n, module] (map vector (range) successor)]
      (if (list? module)
        (let [[module data] module]
          (swap! new-state assoc (+ index n) data)
          module)
        module))))

(defn rewrite
  "Returns [module] or a successor module if module is found in the rules
   mapping. If successor is a function it will be called."
  [rules generation word state new-state new-index [index module]]
  ;(prn :Rewrite index module @new-index)
  (if-let [e (find rules module)]
    (let [successor (val e)
          successor (if (fn? successor)
                      (successor generation word state index module)
                      successor)
          successor (split-successor successor new-state @new-index)]
      (swap! new-index + (count successor))
      successor)
    (do
      (swap! new-index inc)
      [module])))

(defn process
  "Returns new [word state] pair resulting from rewriting each module in the
   original word and updating the properties in state."
  [rules generation word state]
  ;(prn :Process generation word state)
  (let [new-state (atom {})
        new-index (atom 0)
        rewriter (partial rewrite rules generation word state new-state new-index)
        ; Use vec to eliminate laziness since rewriter has side-effects.
        new-word (vec (mapcat rewriter (map vector (range) word)))]
    [new-word @new-state]))

(defn generate
  "Returns a lazy sequence of [word state] pairs, where each subsequent word is
   the result of a rewrite derivation of the preceding word."
  [rules axiom]
  (let [counter (atom 0)
        genproc (fn [[word state]] (process rules (swap! counter inc) word state))
        initial (process {:init axiom} @counter [:init] {})]
    (iterate genproc initial)))

(defn gen
  "Returns a lazy sequence of [word state] pairs for a grammar."
  [{:keys [axiom rules]}]
  (generate rules axiom))



#_(defn context
  "Returns a vector of left / right context values."
  [coll index]
  [(get coll (dec index)) (get coll (inc index))])


(defn age [state index]
  (inc (:age (get state index))))


; When a grammar produces an integer sequence it is named after its identifier
; from "The On-Line Encyclopedia of Integer Sequences" https://oeis.org/

(def grammar
  {:A003849 ; Fibonacci sequence
   {:axiom [0]
    :rules {0 [0 1]
            1 [0]}}
   ;:A005614 ; Fibonacci sequence
   ;{:axiom [1]
   ; :rules {0 [1]
   ;         1 [1 0]}}
   :A010060 ; Thue-Morse sequence
   {:axiom [0]
    :rules {0 [0 1]
            1 [1 0]}}
   :A014577 ; Dragon-curve sequence
   {:axiom [:L]
    :rules {:L [:L :1 :R]
            :R [:L :0 :R]}}
   :A026465 ; Length of n-th run of identical symbols in the Thue-Morse sequence A010060
   {:axiom [1]
    :rules {1 [1 2 1]
            2 [1 2 2 2 1]}}
   :A029883 ; First differences of Thue-Morse sequence A001285
   {:axiom [1]
    :rules {1 [1 0 -1]
            0 [1 -1]
            -1 [0]}}
   :A036577 ; Ternary Thue-Morse sequence
   {:axiom [2]
    :rules {0 [1]
            1 [2 0]
            2 [2 1 0]}}
   :A166253
   {:axiom [1]
    :rules {0 [0 1 1 1 0]
            1 [1 0 0 0 1]}}
   :template
   {:axiom [0]
    :rules {0 []
            1 []}}
   :Rudin-Shapiro-sequence
   {:axiom [:AA]
    :rules {:AA [:AA :AB]
            :AB [:AA :BA]
            :BA [:BB :AB]
            :BB [:BB :BA]}}
   :foo-0
   {:axiom [:A]
    :rules {:A [:B :- :A :- :B]
            :B [:A :+ :B :+ :A]}}
   :foo-1
   {:axiom [:A]
    :rules {:A [:B :- :A :- :B]
            :B (fn [& _] [:A :+ :B :+ :A])}}
   :foo-2
   {:axiom ['(:A {:color :Red})]
    :rules {:A ['(:B {:color :Light-Blue}) :- '(:A {:color :Red}) :- '(:B {:color :Dark-Blue})]
            :B ['(:A {:color :Dark-Red}) :+ (list :B {:color :Blue}) :+ '(:A {:color :Light-Red})]}}
   :foo-3
   {:axiom ['(:A {:age 0})]
    :rules {:A #(vec ['(:B {:age 0}) :- (list %5 {:age (age %3 %4)}) :- '(:B {:age 0})])
            :B (fn [g w s i m] ['(:A {:age 0}) :+ (list m {:age (age s i)}) :+ '(:A {:age 0})])}}
   :foo-4
   {:axiom ['(:A {:age 0})]
    :rules {:A [:B :- :A :- :B]
            :B (fn [g w s i m] [:A :+ :B :+ :A])}}
   })
