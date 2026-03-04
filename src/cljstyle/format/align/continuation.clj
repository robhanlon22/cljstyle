(ns cljstyle.format.align.continuation
  "Multiline continuation padding and standalone comment handling."
  (:require
    [cljstyle.format.align.spacing :as spacing]
    [cljstyle.format.align.syntax :as syntax]
    [cljstyle.format.zloc :as zl]
    [clojure.string :as str]
    [rewrite-clj.zip :as z]))


(defn maybe-indent-standalone-comment
  "Indent a standalone comment to the next substantive node when enabled.

  Comments on the same line as an opening delimiter are left as-is."
  [comment-loc indent-comments? line-has-content?]
  (let [parent (z/up comment-loc)]
    (if (or (not indent-comments?)
            line-has-content?
            (= (first (z/position comment-loc))
               (first (z/position parent))))
      comment-loc
      (let [next-loc (syntax/next-form-node comment-loc)
            target-column (when next-loc (spacing/start-column next-loc))]
        (if (pos-int? target-column)
          (spacing/adjust-left-spacing
            comment-loc
            (- target-column (spacing/start-column comment-loc)))
          comment-loc)))))


(defn- continuation-comment-break?
  "True when a continuation node is a comment that already contains a line break."
  [loc]
  (and (zl/comment? loc)
       (str/includes? (zl/zstr loc) "\n")))


(defn- next-line-start-after-break
  "Return the first node on the next continuation line after a line-break node.

  If the next line begins with whitespace followed by a standalone comment,
  return the comment node so comment lines are padded directly."
  [line-break-loc]
  (let [next-loc (z/next* line-break-loc)
        line-start (cond
                     (or (nil? next-loc) (z/end? next-loc))
                     nil

                     (zl/space? next-loc)
                     (let [content-loc (z/skip z/right* zl/space? next-loc)]
                       (if (or (nil? content-loc) (z/end? content-loc))
                         next-loc
                         (if (zl/comment? content-loc)
                           content-loc
                           content-loc)))

                     :else
                     next-loc)]
    (when-not (or (nil? line-start) (z/end? line-start))
      line-start)))


(defn- next-continuation-line-node
  "Find the next continuation line node while scanning within a multiline value."
  [current-loc]
  (if (continuation-comment-break? current-loc)
    (next-line-start-after-break current-loc)
    (let [current-line (first (z/position current-loc))]
      (loop [loc (z/next* current-loc)]
        (cond
          (or (nil? loc) (z/end? loc))
          nil

          (syntax/newline-node? loc)
          (next-line-start-after-break loc)

          (continuation-comment-break? loc)
          (if (> (first (z/position loc)) current-line)
            loc
            (next-line-start-after-break loc))

          :else
          (recur (z/next* loc)))))))


(defn- first-continuation-line-content
  "Return the first substantive node on a continuation line.

  Leading spaces and commas are skipped; blank lines return nil."
  [line-node]
  (let [content-loc (z/skip
                      z/right*
                      #(or (zl/space? %)
                           (syntax/comma-node? %))
                      line-node)]
    (when (and content-loc
               (not (syntax/newline-node? content-loc)))
      content-loc)))


(defn- continuation-anchor-key
  "Return a stable anchor key describing a continuation content node's parent."
  [content-loc]
  (when-let [parent (z/up content-loc)]
    (let [[line col] (z/position parent)]
      [line col (z/tag parent)])))


(defn- align-content-to-anchor
  "Shift content horizontally to the stored anchor column when needed."
  [content-loc anchor]
  (if (and anchor
           (not= (spacing/start-column content-loc) anchor))
    (spacing/adjust-left-spacing
      content-loc
      (- anchor (spacing/start-column content-loc)))
    content-loc))


(defn- pad-standalone-continuation-comment
  "Apply standalone-comment indentation and preserve continuation padding.

  Comment indentation aligns with the next substantive form when enabled; if a
  next form exists, the continuation padding delta is reapplied."
  [comment-loc padding indent-comments?]
  (if (and indent-comments?
           comment-loc
           (zl/comment? comment-loc))
    (let [next-loc (syntax/next-form-node comment-loc)]
      (cond->
        (maybe-indent-standalone-comment comment-loc true false)
        next-loc
        (spacing/adjust-left-spacing padding)))
    comment-loc))


(defn- pad-continuation-line
  "Pad one continuation line and update the parent-anchor map.

  Returns `[updated-line-node updated-anchors]`."
  [line-node padding indent-comments? anchors]
  (let [line-node (spacing/adjust-left-spacing line-node padding)
        first-content (first-continuation-line-content line-node)
        parent-key (when first-content
                     (continuation-anchor-key first-content))
        comment-line? (and first-content (zl/comment? first-content))
        anchors (if (and parent-key
                         (not comment-line?)
                         (nil? (get anchors parent-key)))
                  (assoc anchors parent-key (spacing/start-column first-content))
                  anchors)
        anchor (when parent-key (get anchors parent-key))
        first-content (if (and first-content
                               (not comment-line?))
                        (align-content-to-anchor first-content anchor)
                        first-content)
        line-node (or first-content line-node)
        first-content (first-continuation-line-content line-node)
        line-node (if (and first-content
                           (zl/comment? first-content))
                    (pad-standalone-continuation-comment
                      first-content
                      padding
                      indent-comments?)
                    line-node)]
    [line-node anchors]))


(defn- pad-multiline-continuations
  "Apply padding to continuation lines inside a multiline node.

  When comment indentation is enabled, standalone comment lines in continuation
  blocks may follow the next substantive line."
  [zloc padding indent-comments?]
  (if-some [line-node (z/down zloc)]
    (loop [line-node line-node
           anchors {}]
      (if-some [next-line-node (next-continuation-line-node line-node)]
        (let [[line-node anchors] (pad-continuation-line
                                    next-line-node
                                    padding
                                    indent-comments?
                                    anchors)]
          (recur line-node anchors))
        line-node))
    zloc))


(defn pad-node-to-column
  "Pad a node to a target start column.

  Applies the same delta to multiline continuation lines."
  [zloc target-column indent-comments?]
  (let [padding (- target-column (spacing/start-column zloc))
        pad-subtree (fn [subtree-loc]
                      (pad-multiline-continuations
                        subtree-loc
                        padding
                        indent-comments?))]
    (-> (spacing/adjust-left-spacing zloc padding)
        (z/subedit-> pad-subtree))))
