(ns com.acrolinx.jug.feb14
  (:require [seesaw.core :as s]
            [seesaw.table :as t]
            [seesaw.bind :as b]))
;; That was a namespace declaration
;; Do not care too much right now

;;; What We Will Cover Today
;; - Clojure primer
;; - Functional aspects
;; - Concurrency built-ins
;; - Swing



;;; Before we Start...
;; - Let's start a REPL
;; - Leiningen, project.clj
;; - Read Eval Print Loop
;; - "Hello World" vs (println "Hello World")
;; - So, today no slides, just interactive coding



;;; And Some of This Will Probably Be Too Fast
;; - I apologize in advance
;; - Just pretend, you can all read this already
;; - Let's see how far we'll get and skip the boring parts
;; - There's plenty of time for questions later
;; - Of course you can shoot your questions anytime
;; - Someone please tell me when 45 minutes passed


;;; Clojure
;; - Published 2007 by Rich Hickey
;; - Clojure 1.0 in May 2009
;; - 1.6 currently at beta1
;; - Astonishingly stable given its age
;; -   JVM            + Lisp
;;   = Great platform + Fantastic tradition
;; - Other projects target JavaScript, .NET, Scheme,
;;   Python, ...
;; - Well known for its concurrency features
;; - See
;;   - http://clojure.org/features
;;   - http://clojure.org/rationale
;;   for many design explanations


;;; IDE
;; - Connect Emacs: cider 
;; - I know nothing but Emacs, sorry
;; - Good plug-ins exist for Eclipse, IntelliJ, vi



;;; What Will Be The Result of This Session?
;; - A rather stupid trading simulation with UI
;;   - Load file interactively, switch namespace
;;   - Run (main-view)
;;         (init-market-and-traders)
;; - And then we will run the simulation



;;; Now Code



;;; Define Some Constants
;; - Special form def defines Vars, a special reference
;;   type
;; - Usually data in Clojure is immutable
;; - Literal numbers
;; - Oh, and the parens are on the *other* side of the
;;   method name...
(def number-of-symbols     30)
(def max-shares-per-symbol 900)
(def number-of-traders     50)
(def number-of-trades      1000)
;; ... of course, we'd better define an entry function
;; which takes these as arguments


;;; Functional Backend


;;; Functional Programming
;; - Clojure is not purely functional, but encourages 
;;   functional programming
;; - No side effects
;; - Higher Order Functions: functions taking functions
;;   as args and returning them, too
;; - Classics: map, filter, reduce


;;; A First Function
;; - Create functions: defn
;; - Apply function to every element and
;;   collect results: map
;; - #() is a function literal, just short for fn,
;;   % is arg
;; - Anonymous functions
;; - Run range, format at the repl
;; - Run it at the REPL
;; - vec creates Vector, syntax: []
;; - ... take your time to parse this
(defn gen-stock-symbols [n-syms]
  (vec (map #(format "%04d" %) (range n-syms))))


;;; Create a Market
;; - Look at the indentation
;; - Hash map literal: {}
;; - Function into adds things to first arg
;;    (into {} [{:a 1 :b 2} {:c 3 :a 4}])
;;   The colon creates keywords
;; - fn creates anonymous function again
;; - Here creates a closure which captures local
;;   bindings
;; - Hey, run it at the REPL
;;    (gen-new-market (gen-stock-symbols 3) 10)
;; - Save one for later (def m ...)
(defn gen-new-market [syms max-share]
  (into {}
        (map (fn [sym]
               {sym (rand-int max-share)})
             syms)))

;;; Reduce
;; Everyone knows reduce?
;; (reduce + [1 2 3 4])
;; (reductions conj [] [1 2 3 4])
;; vals returns all the values in a hash-map
(defn count-shares-in-market [mkt]
  (reduce + (vals mkt)))


;;; Quick Introduction to Sequences
;; - Abstraction: first, all the rest
;; - seq creates a sequence
;; - Huge library on this abstraction
;; - Lazy evaluation
;; - Try it on the saved market
(defn select-from-market [mkt]
  (rand-nth (seq mkt)))


;;; Now We Need Some Traders
;; - mapv creates a vector, too
;; - Portfolio will be a map from market symbols
;;   to counts
;; - Almost trivial, nothing really new here
(defn gen-traders [n-trad]
  (mapv
   (fn [i]
     {:portfolio {} :id i})
   (range n-trad)))

;;; Threading And The Power of Macros
;; - -> is threading, similar to chaining
;; - Threads results as first arg to next
;; - (vals (:portfolio trd))
;; - Keywords are funtions that look themselves up
;; - A macro!
;; - Works at code level
;; - Homoiconicity
(defn count-shares-of-trader [trd]
  (reduce + (-> trd :portfolio vals)))

(defn count-shares-of-traders [trds]
  (reduce + (map count-shares-of-trader trds)))

;;; Sidenote: Random Numbers
;; - To be honest, rand-nth sucks
;; - Linear time (count)
;; - Only one RNG
;; - But good enough for today
(defn select-from-trader [t]
  (rand-nth (seq (:portfolio t))))

;;; Validation
;; - Later we will want to validate
;; - Returns true or false
(defn validate-number-of-shares [expected-val mkt trds]
  (= expected-val
     (+ (count-shares-in-market mkt)
        (count-shares-of-traders trds))))


;;; Introduce Some Global State
;; - Beware the state!


;;; This Should Be a Longer Story
;; - Data is immutable by default (scalars as well as
;;   data structures)
;; - But there are reference types
;; - Each with special semantics for change in a
;;   multi-threaded context
;; - Will see them in action in a minute
;; - Identity, Value, State
;; - See http://clojure.org/state


;;; Var
;; - We already had lots of Vars
;; - Dynamic binding must be declared
;;   (Metadata ... yet another beast)
;; - Then thread local re-binding
;; - To allow REPL we need to be able to really change
;;   them
;; - Not really suitable for user data


;;; Atom
;; - An atom changes state ... uh, atomically
;; - We will calculate the sum once after creating
;;   the market
(def market-sum (atom 0))


;;; Ref
;; - Refs are for transactional change of
;;   an ensemble of identities (coordinated change)
;; - STM - Software Transactional Memory
;; - Only possible by several aspects of Clojure
;;   working together (another talk)
;;   - http://www.heise.de/developer/artikel/Parallelprogrammierung-mit-Clojure-1170690.html
;;     (shameless self-plug)
;; - Trading means take from market and add to trader
;; - Must never see inconsistent state
;; - Use Refs with Clojure's data types
(def market  (ref {}))
(def traders (ref {}))


;;; Agent
;; - Agents are somewhat similar to actors
;;   (but in-memory and readable)
;; - They queue functions we send to them
;; - Functions are run in a dedicated
;;   thread pool
;; - We create a flag for the system to
;;   run or stop
(def system-running-agt (agent "stopped"))
;; - This agent count validation errors
(def validation-agt (agent 0))
;; - To show progress later
(def remaining-agt (agent 0))



;;; Dereference
;; - We need to dereference a reference type to
;;   get its value: deref or @
;; - This fn has a side effect: reads global state
;; - Will be used in a way that it receives an
;;   argument, that we ignore
;; - Use _ to signal that, nothing special
(defn count-shares-in-global-market [_]
  (count-shares-in-market (deref market)))



;;; Buy And Sell: Transactions On Refs
(defn buy [trader-id]
  ;; This is the transaction
  ;; Only reading can do without dosync
  (dosync
   ;; deref/@ reads
   (let [trader (nth @traders trader-id)
         ;; Destructuring bind: assign directly to 
         ;; elements of a vector.  Can do much more, 
         ;; e.g. extract keys from maps. And second 
         ;; deref will always see consistent state
         ;; since running in a transaction
         [sym in-market] (select-from-market @market)]
     (when (< 0 in-market)
       ;; - Alter takes a ref and an update function
       ;; - Update fn gets current value of ref and
       ;;   returns new value
       ;; - Update-in dives into nested associatives
       ;;   and calls a fn with the old val to calculate
       ;;   the new value:
       ;;   (update-in {:a [{} {:b 0}]} [:a 1 :b] inc)
       (alter market #(update-in % [sym] dec))
       ;; - A second write
       ;; - Either both will succeed or none
       (alter traders
              ;; Here update-fn with arg vector
              (fn [t]
                (let [ks [trader-id :portfolio sym]]
                  (update-in t
                             ks
                             #(inc (or % 0))))))))
   ;; Notify the countdown
   ;; Only once per transaction: agents are integrated
   (send-off remaining-agt dec))
   trader-id)


;; Basically the same with dec, inc swapped and
;; different guards
(defn sell [trader-id]
  (dosync
   (let [trader (nth @traders trader-id)
         [sym in-portf] (select-from-trader trader)]
     (when (and (not (nil? sym))
                (not (nil? in-portf))
                (< 0 in-portf))
       (alter market #(update-in % [sym] inc))
       (alter traders
              #(update-in % [trader-id :portfolio sym] dec))))
   ;; Notify the countdown
   (send-off remaining-agt dec))
  trader-id)

(defn init-market-and-traders []
  ;; Ugh, this fn is full of side effects
  (let [syms (gen-stock-symbols number-of-symbols)]
    (dosync
     (alter market
            (fn [_] 
              (gen-new-market syms max-shares-per-symbol)))
     (alter traders (fn [_] 
                      (gen-traders number-of-traders))))
    (swap! market-sum count-shares-in-global-market)))



;;; Now we can test all this at the REPL
(comment
  (init-market-and-traders)
  @market
  (first @traders)
  (buy 0)
  (first @traders))



;;; Do The Validation
;; - Separate agent which keeps counting the overall
;;   sum of all shares

;; An agent function gets the current state of the
;; agents as first arg: cur
(defn validation-agt-fn [cur]
  ;; First take a look at the agent indicating a running 
  ;; system
  (if (not (= @system-running-agt "running"))
    cur
    ;; - Here, we are only reading
    ;; - Need a consistent snapshot
    ;; - Replace with do for errors later
    (dosync
     ;; - Special variable *agent* is the current agent
     ;; - Send sends a fn to an agent:
     ;; - This fn sends itself to the current agent again
     (send *agent* validation-agt-fn)
     ;; Three derefs, coordinated or not?
     (if-not (validate-number-of-shares @market-sum
                                        @market
                                        @traders)
       (do
         (println "Invalid number of shares!")
         ;; Return increased count of errors as new state 
         ;; of agent
         (inc cur))
       cur))))


;;; Run It All
(defn run-stock-exchange []
  ;; Reset validation errors to 0
  (send validation-agt (constantly 0))
  ;; Reset the count down
  (send remaining-agt (constantly (* number-of-traders
                                     number-of-trades)))
  ;; Create a new market and some traders
  (init-market-and-traders)
  ;; Set the system flag
  (send system-running-agt (constantly "running"))
  ;; And wait for it
  (await system-running-agt)
  ;; Start the endless validation loop (while running)
  (send validation-agt validation-agt-fn)

  ;; Create one agent per trader
  (let [agts (vec (map #(agent %) 
                       (range number-of-traders)))]
    ;; Looping construct
    (println "starting agents")
    (doseq [a agts]
      (dotimes [i number-of-trades]
        ;; Send trader agent buy or sell
        (send a (if (= 2 (mod i 3)) buy sell))))
    (println "waiting for agents to finish")
    (apply await agts))

  ;; Shut down the system again
  (send  system-running-agt (constantly "stopped"))
  (await system-running-agt)
  (await validation-agt)
  (await remaining-agt))
;; ... and that is it!


;;; Do we still have time?
;; - Else goto "Activities in The Clojure Community"



;;; Now For Some Fun With Swing
;; But ... (not (= :Swing :Fun))
;; Sure, but Seesaw helps a lot


;;; Market Table
(defn market-table-model [mkt]
  ;; t/ is the namespace alias for seesaw.table
  (t/table-model
   ;; Keys used to extract data from rows
   :columns [:share :count]
   :rows ;; Vec of hash-maps acts as table model
   (vec
    ;; sort-by extracts thing to be sorted by
    (sort-by :share
             (map (fn [[s c]] {:share s :count c})
                  mkt)))))

;; Now this becomes almost too trivial
(defn market-table []
  (s/scrollable
   ;; Note the :id, we'll need it later
   (s/table :id :market-table
            :model [])))

;; - Just add a heading
;; - String becomes label
(defn market-panel []
  (s/vertical-panel :items ["Market" (market-table)]))


;;; The Trader Table
(defn trader-table-model [trds]
  (t/table-model
   :columns [:trader :sum]
   :rows (vec
          (sort-by :trader
                   (map
                    ;; Destructuring on hash maps
                    (fn [{:keys [id portfolio] :as trd}]
                      {:trader id
                       :sum (count-shares-of-trader trd)})
                    trds)))))

(defn trader-table []
  (s/scrollable
   (s/table :id    :trader-table
            :model [])))

(defn trader-panel []
  (s/vertical-panel 
   :items ["Public Float" (trader-table)]))


;;; Show some basic information
(defn info-panel []
  (s/vertical-panel
   :items ["Info"
           "System:"
           (s/label :id   :system-running
                    :text (str @system-running-agt))
           (s/separator)
           "Validation:"
           (s/label :id   :validation
                    :text (str @validation-agt))
           :separator ; shortcut
           "Remaining:"
           (s/label :id   :remaining
                    :text (str @remaining-agt))
           :separator
           (s/button :id :run :text "Run")]))


;;; Binding
;; - So far we only defined layout
;; - And added some identifiers
;; - Seesaws binding mechanism allows us
;;   to add behavior later
;; - But ...


;;; What a Pity: Support For Refs Is Missing In Seesaw
;; - Expression Problem
;; - Protocols, Records and Types
;; - This is kinda cool, since it demoes how
;;   we can solve the expression problem
;; - We can extend a protocol (think interface)
;;   without having access to the class or the
;;   interface
;; - Ignore the implementation for now, it's just
;;   cool that we can do this
(extend-protocol b/Bindable
  clojure.lang.Ref
  (subscribe [this handler]
    (let [key (keyword (gensym "bindable-ref-watcher"))]
      (add-watch this key
                 (fn bindable-ref-watcher
                   [k r o n] 
                   (when-not (= o n) (handler n))))
      (fn [] (remove-watch this key))))
  (notify [this v] (dosync (ref-set this v))))


;;; Binding Continued
;; - Behaviour decoupled from the UI
;; - Find the widget with select
;; - Wiring things in the background with bind
(defn behave [root]
  (b/bind market ; whenever market changes
          ;; transform value using market-table-model
          (b/transform market-table-model)
          ;; take care of the swing thread
          (b/notify-later)
          ;; and update the tabel model
          ;; of the table we find in the root
          ;; widget using the :id we gave it
          (b/property (s/select root [:#market-table])
                      :model))
  (b/bind traders
          (b/transform trader-table-model)
          (b/notify-later)
          (b/property (s/select root [:#trader-table])
                      :model))
  (b/bind validation-agt
          (b/notify-later)
          (b/property (s/select root [:#validation])
                      :text))
  ;; Boring repetion: usually we'd hide this
  ;; with a macro
  (b/bind system-running-agt
          (b/notify-later)
          (b/property (s/select root [:#system-running])
                      :text))
  (b/bind remaining-agt
          (b/notify-later)
          (b/property (s/select root [:#remaining])
                      :text))
  (s/listen (s/select root [:#run])
            :action
            (fn [event]
              (.start (Thread. run-stock-exchange)))))

;;; Panes
;; Put all the panels in split views
(defn main-view-content []
  (s/left-right-split
   (s/left-right-split (market-panel)
                       (trader-panel))
   (info-panel)))


;;; Frame
;; - Finally create a frame
;; - And add the behavior
(defn main-view []
  (s/invoke-now
   (-> (s/frame
        :title "SSE - Stupid Stock Exchange"
        :size [900 :by 500]
        :content (main-view-content))
     s/pack!
     s/show!
     behave)))


;;; Still time left?!?
;; - Else goto "Activities in The Clojure Community"

;;; Speed
;; - Restart REPL
;; - Run simulation at REPL
;;       (time (run-stock-exchange))
;; - Start UI (main-view) and re-run previous command
;; - Slower
;; - Much time spent updating the view
;; - Another solution would be to start a timer and take
;;   snapshots to be displayed.
;; - Wanted to show off seesaw's bind :)
;; - Stays slow after closing the frame because
;;   implemented as watchers
;; - Can remove the watchers
;; - Run
;;      (remove-behavior)
;;      (time (run-stock-exchange))
;; - Could add this to the close button of the JFrame
(defn remove-behavior []
  (doseq [r [validation-agt remaining-agt 
             system-running-agt
             market traders]]
    ;; Java Interop calling .getWatches on reference
    (doseq [k (keys (.getWatches r))]
      (remove-watch r k))))


;;; Activities in The Clojure Community
;; - About once per year something amazing happens
;;   - ClojureScript (2011)
;;   - Datomic (2012)
;;   - Reducers (2012)
;;   - core.async (2013)
;; - Many interesting projects
;;   - Web dev: ring, compojure, enlive, hiccup, etc
;;   - Web with batteries included: Immutant, Pedestal
;;   - Math: incanter
;;   - Live music coding: Overtone
;;   - Storm, Pallet, Light Table, core.logic,
;;     core.match, Om, ...
;; - Talks and Conferences
;;   - See content online for
;;     Strange Loop, Clojure Conj, Clojure West
;;   - Visit EuroClojure (was 2013 in Berlin)
;;   - Simple Made Easy
;;     http://www.infoq.com/presentations/Simple-Made-Easy
;;   - Are We There Yet?
;;     http://www.infoq.com/presentations/Are-We-There-Yet-Rich-Hickey
;;   - Hammock Driven Development
;;     http://www.youtube.com/watch?v=f84n5oFoZBc


;;; Thanks for your time and attention!
;; http://acrolinx.com
;;   ([Job Spam] Sure, we're hiring :-)
;; http://clojure-buch.de
;; https://plus.google.com/+StefanKamphausen/
