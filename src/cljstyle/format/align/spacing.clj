(ns cljstyle.format.align.spacing
  "Column and spacing math for alignment edits."
  (:require
    [cljstyle.format.align.syntax :as syntax]
    [cljstyle.format.zloc :as zl]
    [clojure.string :as str]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))


(defn start-column
  "Return the zero-based starting column of a node."
  [zloc]
  (-> zloc z/position second dec))


(defn node-end-position
  "Return the right-edge column of a node, including trailing comma width.

  For multiline nodes, this returns the width of the final physical line."
  [zloc]
  (let [[_ col] (z/position zloc)
        right (z/right* zloc)
        node-string (zl/zstr zloc)
        comma-width (if (syntax/comma-node? right)
                      (count (n/string (z/node right)))
                      0)
        line-break (str/last-index-of node-string "\n")
        line-width (if line-break
                     (- (count node-string) (inc line-break))
                     (+ (dec col) (count node-string)))]
    (+ line-width comma-width)))


(defn spacing-delta
  "Return effective left-spacing delta after separator-space constraints."
  [desired-delta has-left-space? left-space-width]
  (if has-left-space?
    (let [new-width (max 1 (+ left-space-width desired-delta))]
      (- new-width left-space-width))
    (if (pos? desired-delta)
      desired-delta
      0)))


(defn adjust-left-spacing
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


(defn node-left-space-width
  "Return immediate left-space width for a node, or nil if absent."
  [zloc]
  (when-let [left (z/left* zloc)]
    (when (zl/space? left)
      (count (n/string (z/node left))))))
