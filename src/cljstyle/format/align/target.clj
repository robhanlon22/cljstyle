(ns cljstyle.format.align.target
  "Form-kind and start-node resolution for alignment strategies."
  (:require
    [cljstyle.format.align.syntax :as syntax]
    [cljstyle.format.zloc :as zl]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))


(defn- binding-vector?
  "True if vector is the first binding vector for a configured binding head."
  [zloc rule-config]
  (and (z/vector? zloc)
       (let [parent (z/up zloc)]
         (when (z/list? parent)
           (when-let [head-loc (syntax/first-form-node parent)]
             (and (contains? (:binding-form-set rule-config) (z/string head-loc))
                  (= zloc (syntax/next-form-node head-loc))))))))


(defn- indent-rule->skip-count
  "Extract clause skip count from one indentation rule option vector."
  [opts]
  (let [method (first opts)]
    (if (and (sequential? method)
             (number? (second method)))
      (second method)
      0)))


(defn- compile-clause-skip-matcher
  "Compile indentation rules into exact, unqualified, and regex skip matchers.

  Rule ordering is stable and preserves first-match precedence."
  [indents]
  (reduce
    (fn compile-rule
      [{:keys [exact unqualified] :as matcher} [rule-key opts]]
      (let [skip (indent-rule->skip-count opts)]
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
  (let [head-string (-> zloc syntax/first-form-node z/string)
        skip-count (resolve-clause-skip-count head-string skip-matcher clause-form-skips)]
    (loop [zloc (-> zloc syntax/first-form-node syntax/next-form-node)
           skip skip-count]
      (if (and zloc (pos? skip))
        (recur (syntax/next-form-node zloc) (dec skip))
        zloc))))


(defn node-alignment-kind
  "Return the alignment strategy keyword for node, or nil."
  [zloc rule-config]
  (cond
    (and (contains? (:target-set rule-config) :maps)
         (or (z/map? zloc)
             (= :namespaced-map (n/tag (z/node zloc)))))
    :map

    (and (contains? (:target-set rule-config) :bindings)
         (binding-vector? zloc rule-config))
    :binding

    (and (contains? (:target-set rule-config) :clauses)
         (z/list? zloc)
         (contains? (:clause-form-skips rule-config)
                    (when-let [head (syntax/first-form-node zloc)]
                      (z/string head))))
    :clause

    (and (contains? (:target-set rule-config) :reader-conditionals)
         (zl/reader-conditional? (z/up zloc))
         (contains? #{:list :vector :map} (z/tag zloc)))
    :reader-conditional

    :else
    nil))


(defn- map-alignment-start-node
  "Return first alignable entry in a map or namespaced map form."
  [zloc]
  (if (z/map? zloc)
    (syntax/first-form-node zloc)
    (-> zloc
        z/down
        (#(z/skip z/right* (complement z/map?) %))
        syntax/first-form-node)))


(defn resolve-start-node-for-kind
  "Resolve the first alignable node for the selected alignment strategy."
  [zloc alignment-kind rule-config rules-config]
  (cond
    (= :map alignment-kind)
    (map-alignment-start-node zloc)

    (or (= :binding alignment-kind)
        (= :reader-conditional alignment-kind))
    (syntax/first-form-node zloc)

    :else
    (let [skip-matcher (clause-skip-matcher
                         (get-in rules-config [:indentation :indents]))
          clause-form-skips (:clause-form-skips rule-config)]
      (first-alignable-clause-node zloc skip-matcher clause-form-skips))))
