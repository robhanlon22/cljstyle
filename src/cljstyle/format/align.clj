(ns cljstyle.format.align
  "Vertical alignment of adjacent columns within blank-line groups."
  (:require
    [cljstyle.format.zloc :as zl]
    [clojure.string :as str]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))


(def ^:private binding-forms
  #{"binding" "doseq" "for" "if-let" "if-some" "let" "loop"
    "when-let" "when-some" "with-local-vars" "with-open" "with-redefs"})


(def ^:private clause-forms
  {"case" 1, "cond" 0, "cond->" 1, "cond->>" 1, "condp" 2})


(defn- qualify-core-forms
  [forms]
  (into forms
        (for [prefix ["clojure.core/" "cljs.core/"]
              [form value] forms]
          [(str prefix form) value])))


(defn- prepare-config
  [config]
  (let [excluded (:exclude-forms config)]
    (assoc config
           :targets (get config :targets #{:maps :bindings :clauses :reader-conditionals})
           :indent-comments? (get config :indent-comments? true)
           :bindings (apply disj
                            (into (set (keys (qualify-core-forms (zipmap binding-forms (repeat true)))))
                                  (:extra-binding-forms config))
                            excluded)
           :clauses (apply dissoc
                           (merge (qualify-core-forms clause-forms)
                                  {"are" 2, "clojure.test/are" 2, "cljs.test/are" 2}
                                  (:extra-clause-forms config))
                           excluded))))


(def ^:private trivia-tags
  #{:whitespace :newline :comma :comment})


(defn- trivia?
  [node]
  (contains? trivia-tags (n/tag node)))


(defn- first-form
  [node]
  (first (drop-while #(or (trivia? %) (= :uneval (n/tag %))) (n/children node))))


(defn- form-name
  [node]
  (when node
    (cond
      (contains? #{:meta :meta*} (n/tag node))
      (recur (last (remove trivia? (n/children node))))

      (= :token (n/tag node))
      (n/string node))))


(defn- argument-role
  "Describe the first argument when its parent makes it an alignment target."
  [node config role]
  (let [tag (n/tag node)]
    (cond
      (contains? #{:meta :meta*} tag)
      role

      (and (= :list tag)
           (contains? (:bindings config) (form-name (first-form node))))
      :binding

      (and (= :reader-macro tag)
           (contains? #{"?" "?@"} (form-name (first-form node))))
      :reader-conditional)))


(defn- skip-count
  "Return the number of head forms to skip, or nil for a non-target."
  [node {:keys [targets clauses]} role]
  (cond
    (and (contains? targets :maps) (= :map (n/tag node)))
    0

    (and (contains? targets :bindings) (= :binding role) (= :vector (n/tag node)))
    0

    (and (contains? targets :reader-conditionals)
         (= :reader-conditional role)
         (contains? #{:list :vector :map} (n/tag node)))
    0

    (and (contains? targets :clauses) (= :list (n/tag node)))
    (when-let [head (first-form node)]
      (when-some [skip (get clauses (form-name head))]
        (inc skip)))))


(defn- ignored-node?
  [node]
  (and (contains? #{:meta :meta* :list :uneval} (n/tag node))
       (zl/ignored-form? (z/edn* node))))


(defn- replace-children
  "Discard stale parser positions after changing a node's children."
  [node children]
  (vary-meta (n/replace-children node children)
             dissoc :row :col :next-row :next-col))


(defn- text-extent
  [^String text]
  (let [last-break (.lastIndexOf text "\n")]
    (if (neg? last-break)
      [0 (count text)]
      [(reduce (fn [lines character]
                 (if (= \newline character) (inc lines) lines))
               0 text)
       (- (count text) last-break 1)])))


(defn- node-extent
  "Return [newline-count final-line-width] without rendering entire collections
  or trusting stale parser positions."
  [node]
  (if (n/inner? node)
    (let [[lines column]
          (reduce
            (fn [[lines column] child]
              (let [[child-lines width] (node-extent child)]
                [(+ lines child-lines)
                 (if (zero? child-lines) (+ column width) width)]))
            [0 (n/leader-length node)]
            (n/children node))]
      ;; Reader-prefix wrappers have no closing delimiter of their own.
      [lines (if (contains? #{:list :vector :map :set :fn} (n/tag node))
               (inc column)
               column)])
    (text-extent (n/string node))))


(defn- splice-spaces
  "Expand pending whitespace insertions and removals. Vectors hold edits so child
  indexes remain stable until all spacing changes have been applied."
  [children]
  (into [] (mapcat #(if (vector? %) % [%])) children))


(defn- spaces
  [width]
  (if (pos? width) (n/spaces width) []))


(defn- minimum-indent
  "Bound a leftward move so continuation lines never cross column zero."
  [node]
  (if (or (not (n/inner? node)) (ignored-node? node))
    Long/MAX_VALUE
    (loop [nodes (seq (n/children node)), line-start? false, minimum Long/MAX_VALUE]
      (if-let [child (first nodes)]
        (let [tag (n/tag child)
              newline? (= :newline tag)
              space? (= :whitespace tag)
              blank-space? (and space? (= :newline (some-> nodes second n/tag)))
              indent (if (and line-start? (not (or newline? blank-space?)))
                       (if space? (count (n/string child)) 0)
                       Long/MAX_VALUE)]
          (recur (next nodes)
                 (or newline?
                     (and (= :comment tag) (str/ends-with? (n/string child) "\n")))
                 (long (min minimum indent (minimum-indent child)))))
        (if (and line-start? (contains? #{:list :vector :map :set :fn} (n/tag node)))
          0
          minimum)))))


(defn- shift-continuations
  "Move whitespace on subsequent lines without entering literals or ignored forms."
  [node delta]
  (if (or (zero? delta) (not (n/inner? node)) (ignored-node? node))
    node
    (let [children
          (loop [nodes (seq (n/children node))
                 line-start? false
                 result []]
            (if-let [child (first nodes)]
              (let [tag (n/tag child)
                    newline? (= :newline tag)
                    space? (= :whitespace tag)
                    blank-space? (and space? (= :newline (some-> nodes second n/tag)))
                    result (if (and line-start? (not (or space? newline?)) (pos? delta))
                             (conj result (n/spaces delta))
                             result)
                    child (if (and line-start? space? (not blank-space?))
                            (spaces (+ (count (n/string child)) delta))
                            (shift-continuations child delta))]
                (recur (next nodes)
                       (or newline?
                           (and (= :comment tag) (str/ends-with? (n/string child) "\n")))
                       (conj result child)))
              (cond-> result
                (and line-start? (pos? delta)
                     (contains? #{:list :vector :map :set :fn} (n/tag node)))
                (conj (n/spaces delta)))))]
      (replace-children node (splice-spaces children)))))


(defn- ends-row?
  [tag lines]
  (or (= :newline tag) (and (= :comment tag) (pos? lines))))


(defn- child-layout
  "Measure immediate children once. Columns are zero-based."
  [children start-column]
  (loop [index 0, line 0, column start-column, content? true, result []]
    (if (< index (count children))
      (let [node (nth children index)
            tag (n/tag node)
            [lines width] (node-extent node)
            end (if (zero? lines) (+ column width) width)]
        (recur (inc index) (long (+ line lines)) end
               (cond
                 (ends-row? tag lines) false
                 (= :whitespace tag) content?
                 :else true)
               (conj result {:index index :node node :tag tag
                             :line line :start column :end end :lines lines
                             :line-start? (not content?)
                             :blank? (and (= :newline tag)
                                          (or (> lines 1) (not content?)))})))
      result)))


(defn- alignment-groups
  "Collect substantive sibling rows, honoring newlines inside comment nodes."
  [layout skip]
  (loop [items (seq layout), skip skip, row [], group [], groups []]
    (if-let [{:keys [tag lines blank?] :as cell} (first items)]
      (cond
        (ends-row? tag lines)
        (let [group (cond-> group (seq row) (conj row))
              blank? (or blank? (> lines 1))]
          (recur (next items) skip []
                 (if blank? [] group)
                 (cond-> groups (and blank? (seq group)) (conj group))))

        (or (contains? trivia-tags tag) (and (pos? skip) (= :uneval tag)))
        (recur (next items) skip row group groups)

        (pos? skip)
        (recur (next items) (dec skip) row group groups)

        :else
        (let [comma (second items)
              cell (cond-> cell
                     (= :comma (:tag comma))
                     (assoc :comma-width (count (n/string (:node comma)))))
              cell (update cell :end + (get cell :comma-width 0))]
          (recur (next items) skip (conj row cell) group groups)))
      (let [group (cond-> group (seq row) (conj row))]
        (cond-> groups (seq group) (conj group))))))


(defn- column-target
  "Start after every preceding cell, far enough right to fit continuation lines."
  [row-ends cells]
  (let [after-widest (inc (reduce max (map #(nth row-ends (first %)) cells)))]
    (reduce
      (fn [target [_ {:keys [node start lines]}]]
        (if (and (pos? lines) (< target start))
          (max target (- start (minimum-indent node)))
          target))
      after-widest cells)))


(defn- align-cell
  "Move a cell to its column target and carry its ending position to the next column."
  [target [children row-ends row-shifts] [row {:keys [index node start end lines comma-width]}]]
  (let [shift (nth row-shifts row)
        left (when (pos? index) (nth children (dec index)))
        gap-width (if (= :whitespace (some-> left n/tag))
                    (count (n/string left))
                    0)
        ;; Keep at least one separating space, inserting it when absent.
        delta (max (- 1 gap-width) (- target (+ start shift)))
        movement (+ shift delta)
        moved (if (pos? lines) (shift-continuations node movement) node)
        children (assoc children index moved)
        children (if (pos? gap-width)
                   (assoc children (dec index) (n/spaces (+ gap-width delta)))
                   (assoc children index [(n/spaces delta) moved]))
        new-end (if (zero? lines)
                  (+ end movement)
                  (+ (second (node-extent moved)) (or comma-width 0)))]
    [children
     (assoc row-ends row new-end)
     ;; Later cells move relative to their original measured positions.
     ;; For multiline cells this displacement comes from the final line.
     (assoc row-shifts row (- new-end end))]))


(defn- align-column
  [[_ row-ends :as state] [_ cells]]
  (let [target (column-target row-ends cells)]
    (reduce (fn [state cell] (align-cell target state cell)) state cells)))


(defn- align-group
  "Align columns left to right, carrying each row's actual ending position."
  [children rows]
  (let [columns (reduce-kv
                  (fn [columns row cells]
                    (reduce-kv
                      (fn [columns column cell]
                        (if (zero? column)
                          columns
                          (update columns column (fnil conj []) [row cell])))
                      columns cells))
                  (sorted-map) rows)
        row-ends (mapv (comp :end first) rows)]
    (first
      (reduce align-column
              [children row-ends (vec (repeat (count rows) 0))]
              columns))))


(defn- indent-comment
  [children layout {:keys [index node start]} target]
  (let [left (when (pos? index) (nth layout (dec index)))
        delta (- target start)]
    (cond
      (= :whitespace (:tag left))
      (assoc children (dec index) (spaces (+ (count (n/string (:node left))) delta)))

      (pos? delta)
      (assoc children index [(n/spaces delta) node])

      :else
      children)))


(defn- align-comments
  "Align standalone comments with the next sibling form, stopping at blank lines."
  [children start-column]
  (let [layout (child-layout children start-column)]
    (first
      (reduce
        (fn [[children target] {:keys [tag line start line-start? blank?] :as cell}]
          (cond
            blank?
            [children nil]

            (and target (= :comment tag) (pos? line) line-start?)
            [(indent-comment children layout cell target) target]

            (contains? trivia-tags tag)
            [children target]

            :else
            [children start]))
        [children nil]
        (rseq layout)))))


(defn- align-children
  [node column skip indent-comments?]
  (let [children (vec (n/children node))
        start-column (+ column (n/leader-length node))
        children (if (some? skip)
                   (reduce align-group children
                           (alignment-groups (child-layout children start-column) skip))
                   children)
        children (splice-spaces children)
        children (if (and indent-comments? (some #(= :comment (n/tag %)) children))
                   (splice-spaces (align-comments children start-column))
                   children)]
    (if (= children (n/children node))
      node
      (replace-children node children))))


(defn- advance-position
  [[line column] node]
  (let [[lines width] (node-extent node)]
    [(+ line lines) (if (zero? lines) (+ column width) width)]))


(defn- walk
  "Format children before their parent so measured widths include nested edits."
  [node config inside-target? [line column] role]
  (if (or (not (n/inner? node)) (ignored-node? node))
    node
    (let [skip (skip-count node config role)
          inside-target? (or inside-target? (some? skip))
          argument-role (argument-role node config role)
          children (loop [remaining (seq (n/children node))
                          position [line (+ column (n/leader-length node))]
                          form-index 0
                          children []]
                     (if-let [child (first remaining)]
                       (let [substantive? (not (or (trivia? child) (= :uneval (n/tag child))))
                             child-role (when (and substantive? (= 1 form-index)) argument-role)
                             child (walk child config inside-target? position child-role)]
                         (recur (next remaining)
                                (advance-position position child)
                                (if substantive? (inc form-index) form-index)
                                (conj children child)))
                       children))
          node (if (= children (n/children node)) node (replace-children node children))
          indent-comments? (and inside-target? (:indent-comments? config))]
      (if (or (some? skip)
              (and indent-comments? (some #(= :comment (n/tag %)) children)))
        (try
          (align-children node column skip indent-comments?)
          (catch Exception cause
            (throw (ex-info
                     (format "Formatter threw %s at position %d:%d while aligning columns"
                             (.getSimpleName (class cause)) line (inc column))
                     {:type :cljstyle/format-error
                      :fn "cljstyle.format.align/align-children"
                      :line line :column (inc column) :form (n/string node)}
                     cause))))
        node))))


(defn reformat-form
  "Align maps, binding vectors, clauses and reader conditionals.

  Prepare lookups once per call. Alignment preserves literals and ignored forms;
  blank lines delimit groups. The caller controls whether this pass is enabled."
  [form rule-config]
  (let [config (prepare-config rule-config)]
    (if (seq (:targets config))
      (walk form config false [1 0] nil)
      form)))
