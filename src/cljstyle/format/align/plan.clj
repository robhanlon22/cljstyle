(ns cljstyle.format.align.plan
  "Column target planning for align model graphs."
  (:require
    [cljstyle.format.align.spacing :as spacing]))


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
            actual-delta (spacing/spacing-delta
                           desired-delta
                           has-left-space?
                           left-space-width)]
        (if (zero? actual-delta)
          row-shift
          (update row-shift row-id (fnil + 0) actual-delta))))
    row-shift
    destination-cell-ids))


(defn- plan-group-targets
  "Plan destination columns for one alignment group."
  [plan group-id max-column cells cells-by-column constraints]
  (loop [destination-column 1
         row-shift {}
         plan plan]
    (if (> destination-column max-column)
      plan
      (let [source-cell-ids (get constraints [group-id destination-column])
            target-column (required-target-column source-cell-ids cells row-shift)]
        (if (nil? target-column)
          (recur (inc destination-column) row-shift plan)
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
                   (assoc plan [group-id destination-column] target-column))))))))


(defn plan-column-targets
  "Compute final column targets with deterministic lookahead planning."
  [{:keys [cells cells-by-column constraints max-column-by-group]}]
  (reduce-kv
    (fn [plan group-id max-column]
      (plan-group-targets
        plan
        group-id
        max-column
        cells
        cells-by-column
        constraints))
    {}
    max-column-by-group))
