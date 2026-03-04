(ns cljstyle.format.align.walk
  "Collect and apply alignment edits by scanning sibling rows."
  (:require
    [cljstyle.format.align.continuation :as continuation]
    [cljstyle.format.align.spacing :as spacing]
    [cljstyle.format.align.syntax :as syntax]
    [cljstyle.format.zloc :as zl]
    [rewrite-clj.zip :as z]))


(defn- starts-new-alignment-group?
  "True if a newline starts a fresh alignment group.

  Blank lines and lines with no substantive content break groups."
  [newline-loc line-has-content?]
  (or (> (count (re-seq #"\n" (zl/zstr newline-loc))) 1)
      (not line-has-content?)))


(defn- advance-model-on-newline
  "Advance scanner state across a newline, resetting row/column as needed."
  [state newline-loc preserve-prev-on-newline?]
  (let [{:keys [group-id row-id line-has-content? prev-column prev-cell-id]} state
        group-break? (starts-new-alignment-group? newline-loc line-has-content?)
        keep-previous? (and (not group-break?) preserve-prev-on-newline?)]
    (assoc state
           :node-loc (z/right* newline-loc)
           :group-id (if group-break? (inc group-id) group-id)
           :row-id (if group-break? 0 (inc row-id))
           :column 0
           :prev-column (when keep-previous? prev-column)
           :prev-cell-id (when keep-previous? prev-cell-id)
           :line-has-content? false)))


(defn- register-substantive-cell
  "Register one substantive node as an alignment cell and advance scanner state."
  [state node-loc]
  (let [{:keys [group-id row-id column prev-column prev-cell-id next-cell-id
                cells! cells-by-column! constraints! max-column-by-group!]} state
        cell-id next-cell-id
        start (spacing/start-column node-loc)
        end (spacing/node-end-position node-loc)
        left-space-width (spacing/node-left-space-width node-loc)
        has-left-space? (some? left-space-width)
        cell {:group-id group-id
              :row-id row-id
              :column column
              :start start
              :end end
              :has-left-space? has-left-space?
              :left-space-width (or left-space-width 0)}
        cells! (assoc! cells! cell-id cell)
        column-key [group-id column]
        cells-by-column! (assoc!
                           cells-by-column!
                           column-key
                           (conj (get cells-by-column! column-key []) cell-id))
        constraints! (if (some? prev-column)
                       (let [destination-key [group-id (inc prev-column)]]
                         (assoc!
                           constraints!
                           destination-key
                           (conj (get constraints! destination-key []) prev-cell-id)))
                       constraints!)
        max-column-by-group! (assoc!
                               max-column-by-group!
                               group-id
                               (max column
                                    (get max-column-by-group! group-id -1)))]
    (assoc state
           :node-loc (z/right* node-loc)
           :column (inc column)
           :prev-column column
           :prev-cell-id cell-id
           :line-has-content? true
           :next-cell-id (inc next-cell-id)
           :cells! cells!
           :cells-by-column! cells-by-column!
           :constraints! constraints!
           :max-column-by-group! max-column-by-group!)))


(defn collect-alignment-model
  "Scan nodes into alignment cells and adjacency constraints."
  [start {:keys [preserve-prev-on-newline?]
          :or {preserve-prev-on-newline? false}}]
  (loop [state {:node-loc start
                :group-id 0
                :row-id 0
                :column 0
                :prev-column nil
                :prev-cell-id nil
                :line-has-content? false
                :next-cell-id 0
                :cells! (transient {})
                :cells-by-column! (transient {})
                :constraints! (transient {})
                :max-column-by-group! (transient {})}]
    (if-let [node-loc (:node-loc state)]
      (cond
        (zl/space? node-loc)
        (recur (assoc state :node-loc (z/right* node-loc)))

        (syntax/newline-node? node-loc)
        (recur (advance-model-on-newline
                 state
                 node-loc
                 preserve-prev-on-newline?))

        (syntax/comma-node? node-loc)
        (recur (assoc state
                      :node-loc (z/right* node-loc)
                      :line-has-content? true))

        (zl/comment? node-loc)
        (recur (assoc state
                      :node-loc (z/right* node-loc)
                      :line-has-content? true))

        :else
        (recur (register-substantive-cell state node-loc)))
      {:cells (persistent! (:cells! state))
       :cells-by-column (persistent! (:cells-by-column! state))
       :constraints (persistent! (:constraints! state))
       :max-column-by-group (persistent! (:max-column-by-group! state))})))


(defn- apply-node-target-step
  "Apply one comment or substantive-node alignment step."
  [state plan indent-comments?]
  (let [{:keys [node-loc group-id column line-has-content?]} state]
    (if (zl/comment? node-loc)
      (let [comment-loc (continuation/maybe-indent-standalone-comment
                          node-loc
                          indent-comments?
                          line-has-content?)]
        (assoc state
               :node-loc (z/right* comment-loc)
               :line-has-content? true
               :last-loc comment-loc))
      (let [aligned-node (if-some [target (get plan [group-id column])]
                           (continuation/pad-node-to-column
                             node-loc
                             target
                             indent-comments?)
                           node-loc)]
        (assoc state
               :node-loc (z/right* aligned-node)
               :column (inc column)
               :line-has-content? true
               :last-loc aligned-node)))))


(defn apply-column-targets
  "Apply precomputed column targets while preserving group boundaries."
  [start plan {:keys [indent-comments?]}]
  (loop [state {:node-loc start
                :group-id 0
                :column 0
                :line-has-content? false
                :last-loc start}]
    (if-let [node-loc (:node-loc state)]
      (cond
        (zl/space? node-loc)
        (recur (assoc state :node-loc (z/right* node-loc)))

        (syntax/newline-node? node-loc)
        (let [group-break? (starts-new-alignment-group?
                             node-loc
                             (:line-has-content? state))]
          (recur (assoc state
                        :node-loc (z/right* node-loc)
                        :group-id (if group-break?
                                    (inc (:group-id state))
                                    (:group-id state))
                        :column 0
                        :line-has-content? false
                        :last-loc node-loc)))

        (syntax/comma-node? node-loc)
        (recur (assoc state
                      :node-loc (z/right* node-loc)
                      :line-has-content? true))

        :else
        (recur (apply-node-target-step state plan indent-comments?)))
      (:last-loc state))))
