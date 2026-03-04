(ns cljstyle.format.align
  "Formatting rules for vertical alignment in associative forms.

  This rule aligns map entries, binding vectors, clause-style forms, and
  reader conditional bodies by scanning adjacent cells within blank-line
  groups, planning target columns, then applying spacing edits."
  (:require
    [cljstyle.format.align.config :as config]
    [cljstyle.format.align.plan :as plan]
    [cljstyle.format.align.target :as target]
    [cljstyle.format.align.walk :as walk]
    [rewrite-clj.zip :as z]))


(defn- alignable-node?
  "Predicate for nodes supported by alignment."
  [zloc rule-config]
  (let [rule-config (config/effective-config rule-config)]
    (boolean (target/node-alignment-kind zloc rule-config))))


(defn- align-node
  "Apply alignment to one supported node using rule and indent config.

  Clause forms consult indentation rules to derive head argument skip counts."
  ([zloc rule-config]
   (align-node zloc rule-config nil))
  ([zloc rule-config rules-config]
   (let [rule-config (config/effective-config rule-config)
         rules-config (or rules-config {})
         alignment-kind (target/node-alignment-kind zloc rule-config)
         start (target/resolve-start-node-for-kind
                 zloc
                 alignment-kind
                 rule-config
                 rules-config)]
     (if start
       (let [model (walk/collect-alignment-model
                     start
                     {:preserve-prev-on-newline? (= :clause alignment-kind)})
             column-plan (plan/plan-column-targets model)]
         (-> (walk/apply-column-targets start column-plan rule-config)
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
