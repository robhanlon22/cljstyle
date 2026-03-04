(ns cljstyle.format.align.syntax
  "Syntax-level zipper predicates and navigation for alignment."
  (:require
    [cljstyle.format.zloc :as zl]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))


(defn comma-node?
  "True if the node at this location is a comma token."
  [zloc]
  (and zloc (= :comma (n/tag (z/node zloc)))))


(defn newline-node?
  "True if the node at this location is a newline token."
  [zloc]
  (= :newline (n/tag (z/node zloc))))


(def ^:private trivial-tags
  #{:whitespace :comma :newline :comment})


(defn- trivial-node?
  "True if this node is skipped while scanning for alignment cells."
  [zloc]
  (contains? trivial-tags (n/tag (z/node zloc))))


(defn next-form-node
  "Return the next substantive sibling node."
  [zloc]
  (z/skip z/right* trivial-node? (z/right* zloc)))


(defn first-form-node
  "Return the first substantive child node, unwrapping metadata."
  [zloc]
  (-> zloc
      z/down
      (#(z/skip z/right* trivial-node? %))
      zl/unwrap-meta))
