(ns cljstyle.format.align
  "Formatting rules for vertical alignment in associative forms.

  This rule aligns map entries, binding vectors, clause-style forms, and
  reader conditional bodies by scanning adjacent cells within blank-line
  groups, planning target columns, then applying spacing edits."
  (:require
    [cljstyle.format.zloc :as zl]
    [clojure.set :as set]
    [clojure.string :as str]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))


(def ^:private builtin-binding-forms
  [;; Unqualified built-ins.
   "binding"
   "doseq"
   "for"
   "if-let"
   "if-some"
   "let"
   "loop"
   "when-let"
   "when-some"
   "with-local-vars"
   "with-open"
   "with-redefs"

   ;; Qualified Clojure built-ins.
   "clojure.core/binding"
   "clojure.core/doseq"
   "clojure.core/for"
   "clojure.core/if-let"
   "clojure.core/if-some"
   "clojure.core/let"
   "clojure.core/loop"
   "clojure.core/when-let"
   "clojure.core/when-some"
   "clojure.core/with-local-vars"
   "clojure.core/with-open"
   "clojure.core/with-redefs"

   ;; Qualified ClojureScript built-ins.
   "cljs.core/binding"
   "cljs.core/doseq"
   "cljs.core/for"
   "cljs.core/if-let"
   "cljs.core/if-some"
   "cljs.core/let"
   "cljs.core/loop"
   "cljs.core/when-let"
   "cljs.core/when-some"
   "cljs.core/with-local-vars"
   "cljs.core/with-open"
   "cljs.core/with-redefs"])


(def ^:private builtin-clause-forms
  {;; Unqualified built-ins.
   "are" 2
   "case" 1
   "cond" 0
   "cond->" 1
   "cond->>" 1
   "condp" 2

   ;; Qualified Clojure built-ins.
   "clojure.core/case" 1
   "clojure.core/cond" 0
   "clojure.core/cond->" 1
   "clojure.core/cond->>" 1
   "clojure.core/condp" 2
   "clojure.test/are" 2

   ;; Qualified ClojureScript built-ins.
   "cljs.core/case" 1
   "cljs.core/cond" 0
   "cljs.core/cond->" 1
   "cljs.core/cond->>" 1
   "cljs.core/condp" 2
   "cljs.test/are" 2})


(def ^:private default-targets
  #{:maps :bindings :clauses :reader-conditionals})


(def ^:private default-rule-config
  {:enabled? false
   :targets default-targets
   :extra-binding-forms []
   :extra-clause-forms {}
   :indent-comments? true
   :exclude-forms #{}})


(defn- normalize-config
  "Merge defaults and precompute lookup structures for alignment checks.

  Built-in and custom form sets are merged, then excluded forms are removed."
  [rule-config]
  (let [config (merge default-rule-config rule-config)
        excluded (set (:exclude-forms config))
        binding-form-set (set/difference
                           (set (concat builtin-binding-forms
                                        (:extra-binding-forms config)))
                           excluded)
        clause-form-skips (apply dissoc
                                 (merge builtin-clause-forms
                                        (:extra-clause-forms config))
                                 excluded)]
    (assoc config
           :target-set (set (:targets config))
           :binding-form-set binding-form-set
           :clause-form-skips clause-form-skips)))


(def ^:private effective-config
  "Memoized normalized alignment configuration."
  (memoize normalize-config))


(defn- comma-node?
  "True if the node at this location is a comma token."
  [zloc]
  (and zloc (= :comma (n/tag (z/node zloc)))))


(defn- newline-node?
  "True if the node at this location is a newline token."
  [zloc]
  (= :newline (n/tag (z/node zloc))))


(defn- namespaced-map?
  "True if the node at this location is a namespaced map."
  [zloc]
  (= :namespaced-map (n/tag (z/node zloc))))


(def ^:private trivial-tags
  #{:whitespace :comma :newline :comment})


(defn- trivial-node?
  "True if this node is skipped while scanning for alignment cells."
  [zloc]
  (contains? trivial-tags (n/tag (z/node zloc))))


(defn- skip-trivial-nodes
  "Skip whitespace, commas, comments, and newline tokens."
  [zloc move]
  (z/skip move trivial-node? zloc))


(defn- next-form-node
  "Return the next substantive sibling node."
  [zloc]
  (-> zloc z/right* (skip-trivial-nodes z/right*)))


(defn- next-substantive-node
  "Return the next substantive node to the right.

  Used for comment indentation lookahead."
  [zloc]
  (loop [zloc (z/right* zloc)]
    (cond
      (nil? zloc)
      nil

      (trivial-node? zloc)
      (recur (z/right* zloc))

      :else
      zloc)))


(defn- first-form-node
  "Return the first substantive child node, unwrapping metadata."
  [zloc]
  (-> zloc z/down (skip-trivial-nodes z/right*) zl/unwrap-meta))


(defn- start-column
  "Return the zero-based starting column of a node."
  [zloc]
  (-> zloc z/position second dec))


(defn- node-end-position
  "Return the right-edge column of a node, including trailing comma width.

  For multiline nodes, this returns the width of the final physical line."
  [zloc]
  (let [[_ col] (z/position zloc)
        right (z/right* zloc)
        node-string (zl/zstr zloc)
        comma-width (if (comma-node? right)
                      (count (n/string (z/node right)))
                      0)
        line-break (str/last-index-of node-string "\n")
        line-width (if line-break
                     (- (count node-string) (inc line-break))
                     (+ (dec col) (count node-string)))]
    (+ line-width comma-width)))


(defn- spacing-delta
  "Return effective left-spacing delta after separator-space constraints."
  [desired-delta has-left-space? left-space-width]
  (if has-left-space?
    (let [new-width (max 1 (+ left-space-width desired-delta))]
      (- new-width left-space-width))
    (if (pos? desired-delta)
      desired-delta
      0)))


(defn- adjust-left-spacing
  "Adjust immediate left spacing, preserving at least one separating space."
  [zloc delta]
  (let [left (z/left* zloc)]
    (cond
      (zl/space? left)
      (let [left-space-width (-> left z/node n/string count)
            delta (spacing-delta delta true left-space-width)
            width (+ left-space-width delta)]
        (z/right* (z/replace* left (n/spaces width))))

      :else
      (let [delta (spacing-delta delta false 0)]
        (if (pos? delta)
          (z/insert-space-left zloc delta)
          zloc)))))


(defn- node-left-space-width
  "Return immediate left-space width for a node, or nil if absent."
  [zloc]
  (when-let [left (z/left* zloc)]
    (when (zl/space? left)
      (count (n/string (z/node left))))))


(defn- append-cell-id
  "Append a cell id to a transient vector index at key."
  [index! k cell-id]
  (assoc! index! k (conj (get index! k []) cell-id)))


(declare maybe-indent-standalone-comment)


(defn- pad-multiline-continuations
  "Apply padding to continuation lines inside a multiline node.

  When comment indentation is enabled, standalone comment lines in continuation
  blocks may follow the next substantive line."
  [zloc padding indent-comments?]
  (if-some [zloc (z/down zloc)]
    (loop [zloc zloc]
      (if-some [line-node (when-let [newline-loc (z/skip z/next* (complement newline-node?) zloc)]
                            (let [next-loc (z/next* newline-loc)]
                              (when (not (z/end? next-loc))
                                next-loc)))]
        (let [line-node (adjust-left-spacing line-node padding)
              first-content (let [content-loc (z/skip
                                                z/right*
                                                #(or (zl/space? %)
                                                     (comma-node? %))
                                                line-node)]
                              (when (and content-loc
                                         (not (newline-node? content-loc)))
                                content-loc))
              line-node (if (and indent-comments?
                                 (zl/comment? first-content))
                          (maybe-indent-standalone-comment first-content true false)
                          line-node)]
          (recur line-node))
        zloc))
    zloc))


(defn- pad-node-to-column
  "Pad a node to a target start column.

  Applies the same delta to multiline continuation lines."
  [zloc target-column indent-comments?]
  (let [padding (- target-column (start-column zloc))
        pad-subtree (fn [subtree-loc]
                      (pad-multiline-continuations
                        subtree-loc
                        padding
                        indent-comments?))]
    (-> (adjust-left-spacing zloc padding)
        (z/subedit-> pad-subtree))))


(defn- maybe-indent-standalone-comment
  "Indent a standalone comment to the next substantive node when enabled.

  Comments on the same line as an opening delimiter are left as-is."
  [comment-loc indent-comments? line-has-content?]
  (let [parent (z/up comment-loc)]
    (if (or (not indent-comments?)
            line-has-content?
            (= (first (z/position comment-loc))
               (first (z/position parent))))
      comment-loc
      (let [next-loc (next-substantive-node comment-loc)
            target-column (when next-loc (start-column next-loc))]
        (if (pos-int? target-column)
          (adjust-left-spacing
            comment-loc
            (- target-column (start-column comment-loc)))
          comment-loc)))))


(defn- starts-new-alignment-group?
  "True if a newline starts a fresh alignment group.

  Blank lines and lines with no substantive content break groups."
  [newline-loc line-has-content?]
  (or (> (count (re-seq #"\n" (zl/zstr newline-loc))) 1)
      (not line-has-content?)))


(defn- collect-alignment-model
  "Scan nodes into alignment cells and adjacency constraints."
  [start {:keys [preserve-prev-on-newline?]
          :or {preserve-prev-on-newline? false}}]
  (loop [node-loc start
         group-id 0
         row-id 0
         column 0
         prev-column nil
         prev-cell-id nil
         line-has-content? false
         next-cell-id 0
         cells! (transient {})
         cells-by-column! (transient {})
         constraints! (transient {})
         max-column-by-group! (transient {})]
    (if node-loc
      (cond
        (zl/space? node-loc)
        (recur (z/right* node-loc)
               group-id
               row-id
               column
               prev-column
               prev-cell-id
               line-has-content?
               next-cell-id
               cells!
               cells-by-column!
               constraints!
               max-column-by-group!)

        (newline-node? node-loc)
        (let [group-break? (starts-new-alignment-group?
                             node-loc
                             line-has-content?)
              group-id (if group-break? (inc group-id) group-id)
              row-id (if group-break? 0 (inc row-id))]
          (recur (z/right* node-loc)
                 group-id
                 row-id
                 0
                 (when (and (not group-break?)
                            preserve-prev-on-newline?)
                   prev-column)
                 (when (and (not group-break?)
                            preserve-prev-on-newline?)
                   prev-cell-id)
                 false
                 next-cell-id
                 cells!
                 cells-by-column!
                 constraints!
                 max-column-by-group!))

        (comma-node? node-loc)
        (recur (z/right* node-loc)
               group-id
               row-id
               column
               prev-column
               prev-cell-id
               true
               next-cell-id
               cells!
               cells-by-column!
               constraints!
               max-column-by-group!)

        (zl/comment? node-loc)
        (recur (z/right* node-loc)
               group-id
               row-id
               column
               prev-column
               prev-cell-id
               true
               next-cell-id
               cells!
               cells-by-column!
               constraints!
               max-column-by-group!)

        :else
        (let [cell-id next-cell-id
              start (start-column node-loc)
              end (node-end-position node-loc)
              left-space-width (node-left-space-width node-loc)
              has-left-space? (some? left-space-width)
              cell {:group-id group-id
                    :row-id row-id
                    :column column
                    :start start
                    :end end
                    :has-left-space? has-left-space?
                    :left-space-width (or left-space-width 0)}
              cells! (assoc! cells! cell-id cell)
              cells-by-column! (append-cell-id
                                 cells-by-column!
                                 [group-id column]
                                 cell-id)
              constraints! (if (some? prev-column)
                             (append-cell-id
                               constraints!
                               [group-id (inc prev-column)]
                               prev-cell-id)
                             constraints!)
              max-column-by-group! (assoc!
                                     max-column-by-group!
                                     group-id
                                     (max column
                                          (get
                                            max-column-by-group!
                                            group-id
                                            -1)))]
          (recur (z/right* node-loc)
                 group-id
                 row-id
                 (inc column)
                 column
                 cell-id
                 true
                 (inc next-cell-id)
                 cells!
                 cells-by-column!
                 constraints!
                 max-column-by-group!)))
      {:cells (persistent! cells!)
       :cells-by-column (persistent! cells-by-column!)
       :constraints (persistent! constraints!)
       :max-column-by-group (persistent! max-column-by-group!)})))


(defn- required-target-column
  "Compute required target column for a destination column."
  [source-cell-ids cells row-shift]
  (when (seq source-cell-ids)
    (reduce
      (fn [max-target source-cell-id]
        (let [{:keys [row-id end]} (get cells source-cell-id)
              shifted-end (+ end (get row-shift row-id 0))]
          (max max-target (inc shifted-end))))
      0
      source-cell-ids)))


(defn- update-row-shifts
  "Update per-row shifts after applying one destination-column target."
  [row-shift destination-cell-ids target-column cells]
  (reduce
    (fn [row-shift cell-id]
      (let [{:keys [row-id start has-left-space? left-space-width]}
            (get cells cell-id)
            shifted-start (+ start (get row-shift row-id 0))
            desired-delta (- target-column shifted-start)
            actual-delta (spacing-delta
                           desired-delta
                           has-left-space?
                           left-space-width)]
        (if (zero? actual-delta)
          row-shift
          (update row-shift row-id (fnil + 0) actual-delta))))
    row-shift
    destination-cell-ids))


(defn- plan-column-targets
  "Compute final column targets with deterministic lookahead planning."
  [{:keys [cells cells-by-column constraints max-column-by-group]}]
  (reduce-kv
    (fn [plan group-id max-column]
      (loop [destination-column 1
             row-shift {}
             plan plan]
        (if (> destination-column max-column)
          plan
          (let [source-cell-ids (get constraints
                                     [group-id destination-column])
                target-column (required-target-column
                                source-cell-ids
                                cells
                                row-shift)]
            (if (nil? target-column)
              (recur (inc destination-column)
                     row-shift
                     plan)
              (let [destination-cell-ids (get cells-by-column
                                              [group-id destination-column]
                                              [])
                    row-shift (update-row-shifts
                                row-shift
                                destination-cell-ids
                                target-column
                                cells)]
                (recur (inc destination-column)
                       row-shift
                       (assoc plan
                              [group-id destination-column]
                              target-column))))))))
    {}
    max-column-by-group))


(defn- apply-column-targets
  "Apply precomputed column targets while preserving group boundaries."
  [start plan {:keys [indent-comments?]}]
  (loop [node-loc start
         group-id 0
         column 0
         line-has-content? false
         last-loc start]
    (if node-loc
      (cond
        (zl/space? node-loc)
        (recur (z/right* node-loc)
               group-id
               column
               line-has-content?
               last-loc)

        (newline-node? node-loc)
        (let [group-break? (starts-new-alignment-group? node-loc line-has-content?)
              group-id (if group-break? (inc group-id) group-id)]
          (recur (z/right* node-loc)
                 group-id
                 0
                 false
                 node-loc))

        (comma-node? node-loc)
        (recur (z/right* node-loc)
               group-id
               column
               true
               last-loc)

        (zl/comment? node-loc)
        (let [comment-loc (maybe-indent-standalone-comment
                            node-loc
                            indent-comments?
                            line-has-content?)]
          (recur (z/right* comment-loc)
                 group-id
                 column
                 true
                 comment-loc))

        :else
        (let [node-loc (if-some [target (get plan [group-id column])]
                         (pad-node-to-column node-loc target indent-comments?)
                         node-loc)]
          (recur (z/right* node-loc)
                 group-id
                 (inc column)
                 true
                 node-loc)))
      last-loc)))


(defn- binding-vector?
  "True if vector is the first binding vector for a configured binding head."
  [zloc rule-config]
  (and (z/vector? zloc)
       (let [parent (z/up zloc)]
         (when (z/list? parent)
           (when-let [head-loc (first-form-node parent)]
             (and (contains? (:binding-form-set rule-config) (z/string head-loc))
                  (= zloc (next-form-node head-loc))))))))


(defn- compile-clause-skip-matcher
  "Compile indentation rules into exact, unqualified, and regex skip matchers.

  Rule ordering is stable and preserves first-match precedence."
  [indents]
  (reduce
    (fn compile-rule
      [{:keys [exact unqualified] :as matcher} [rule-key opts]]
      (let [method (first opts)
            skip (cond
                   (and (sequential? method)
                        (number? (second method)))
                   (second method)

                   :else
                   0)]
        (cond
          (and (symbol? rule-key) (namespace rule-key))
          (assoc matcher :exact (assoc exact rule-key skip))

          (symbol? rule-key)
          (assoc matcher :unqualified (assoc unqualified rule-key skip))

          (instance? java.util.regex.Pattern rule-key)
          (update matcher :patterns conj [rule-key skip])

          :else
          matcher)))
    {:exact {}
     :unqualified {}
     :patterns []}
    (sort-by
      (fn [[rule-key _]]
        (cond
          (symbol? rule-key)
          (if (namespace rule-key)
            (str 0 rule-key)
            (str 1 rule-key))

          (instance? java.util.regex.Pattern rule-key)
          (str 2 rule-key)

          :else
          (str 3 rule-key)))
      indents)))


(def ^:private clause-skip-matcher
  "Memoized matcher compiler keyed by indentation rules."
  (memoize compile-clause-skip-matcher))


(defn- resolve-clause-skip-count
  "Resolve leading argument skip count for a clause-style head.

  Precedence: exact symbol, unqualified symbol, regex pattern, then config map."
  [head-string skip-matcher clause-form-skips]
  (let [head-sym (symbol head-string)
        base-sym (symbol (name head-sym))
        {:keys [exact unqualified patterns]} skip-matcher]
    (or
      (get exact head-sym)
      (get unqualified base-sym)
      (some
        (fn regex-skip
          [[pattern skip]]
          (when (re-find pattern head-string)
            skip))
        patterns)
      (get clause-form-skips head-string 0))))


(defn- first-alignable-clause-node
  "Locate the first clause node eligible for clause-style alignment."
  [zloc skip-matcher clause-form-skips]
  (let [head-string (-> zloc first-form-node z/string)
        skip-count (resolve-clause-skip-count head-string skip-matcher clause-form-skips)]
    (loop [zloc (-> zloc first-form-node next-form-node)
           skip skip-count]
      (if (and zloc (pos? skip))
        (recur (next-form-node zloc) (dec skip))
        zloc))))


(defn- node-alignment-kind
  "Return the alignment strategy keyword for node, or nil."
  [zloc rule-config]
  (cond
    (and (contains? (:target-set rule-config) :maps)
         (or (z/map? zloc)
             (namespaced-map? zloc)))
    :map

    (and (contains? (:target-set rule-config) :bindings)
         (binding-vector? zloc rule-config))
    :binding

    (and (contains? (:target-set rule-config) :clauses)
         (z/list? zloc)
         (contains? (:clause-form-skips rule-config)
                    (when-let [head (first-form-node zloc)]
                      (z/string head))))
    :clause

    (and (contains? (:target-set rule-config) :reader-conditionals)
         (zl/reader-conditional? (z/up zloc))
         (contains? #{:list :vector :map} (z/tag zloc)))
    :reader-conditional

    :else
    nil))


(defn- alignable-node?
  "Predicate for nodes supported by alignment."
  [zloc rule-config]
  (let [rule-config (effective-config rule-config)]
    (boolean (node-alignment-kind zloc rule-config))))


(defn- align-node
  "Apply alignment to one supported node using rule and indent config.

  Clause forms consult indentation rules to derive head argument skip counts."
  ([zloc rule-config]
   (align-node zloc rule-config nil))
  ([zloc rule-config rules-config]
   (let [rule-config (effective-config rule-config)
         rules-config (or rules-config {})
         alignment-kind (node-alignment-kind zloc rule-config)
         start-fn (cond
                    (= :map alignment-kind)
                    (fn [loc]
                      (if (z/map? loc)
                        (first-form-node loc)
                        (-> loc
                            z/down
                            (#(z/skip z/right* (complement z/map?) %))
                            first-form-node)))

                    (= :binding alignment-kind)
                    first-form-node

                    (= :reader-conditional alignment-kind)
                    first-form-node

                    :else
                    (let [skip-matcher (clause-skip-matcher
                                         (get-in rules-config [:indentation :indents]))
                          clause-form-skips (:clause-form-skips rule-config)]
                      #(first-alignable-clause-node % skip-matcher clause-form-skips)))]
     (if-let [start (start-fn zloc)]
       (let [model (collect-alignment-model
                     start
                     {:preserve-prev-on-newline? (= :clause alignment-kind)})
             plan (plan-column-targets model)]
         (-> (apply-column-targets start plan rule-config)
             z/up))
       zloc))))


(def align-columns
  "Rule to apply vertical column alignment in associative Clojure forms.

  Aligns map entries, binding vectors, clause-style forms, and reader
  conditionals. Configure targets with `:targets`, add forms with
  `:extra-binding-forms` and `:extra-clause-forms`, and opt out forms with
  `:exclude-forms`. Standalone comments can follow the next substantive line
  with `:indent-comments?`. Alignment is column-oriented within blank-line
  groups."
  [:align nil alignable-node? align-node])
