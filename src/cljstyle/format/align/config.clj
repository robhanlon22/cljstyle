(ns cljstyle.format.align.config
  "Configuration normalization for the align rule."
  (:require
    [clojure.set :as set]))


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


(defn normalize-config
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


(def effective-config
  "Memoized normalized alignment configuration."
  (memoize normalize-config))
