(ns cljstyle.format.align-test
  (:require
    [cljstyle.config :as config]
    [cljstyle.format.align :as align]
    [cljstyle.format.core :as core]
    [cljstyle.test-util :as test-util]
    [clojure.string :as str]
    [clojure.test :as test :refer [are deftest is testing]]))


(defn- lines
  "Join fixture lines without trimming their whitespace."
  [text]
  (str/join "\n" text))


(defmethod test/assert-expr 'aligned?
  [msg [_ source expected config]]
  (test/assert-expr
    msg
    `(~'reformatted? align/reformat-form ~(or config {})
                     (lines ~source) (lines ~expected))))


(deftest core-alignment-cases
  (testing "core alignment behavior"
    (are [source expected]
         (aligned? source expected)

      ["{:ram \"warm\""
       " :region \"dry\"}"]
      ["{:ram    \"warm\""
       " :region \"dry\"}"]

      ["(let [ram \"warm\""
       "      region \"dry\"]"
       "  [ram region])"]
      ["(let [ram    \"warm\""
       "      region \"dry\"]"
       "  [ram region])"]

      ["(cond"
       "  done? 1"
       "  session-expired? 2)"]
      ["(cond"
       "  done?            1"
       "  session-expired? 2)"]

      ["(cond-> build"
       "  valid? (emit build)"
       "  critical? (stream build))"]
      ["(cond-> build"
       "  valid?    (emit build)"
       "  critical? (stream build))"]

      ["(case kind"
       "  :brief 1"
       "  :larger 22"
       "  (fallback-with-super-long-name kind))"]
      ["(case kind"
       "  :brief  1"
       "  :larger 22"
       "  (fallback-with-super-long-name kind))"]

      ["{:omega 1"
       " :x 2}"]
      ["{:omega 1"
       " :x     2}"]

      ["(cond compile clean ahead"
       "      ci test run"
       "      time fix"
       "      process phase finish)"]
      ["(cond compile clean ahead"
       "      ci      test  run"
       "      time    fix"
       "      process phase finish)"]

      ["{:project {:id :id"
       "           :domain :east}"
       " :domain :east}"]
      ["{:project {:id     :id"
       "           :domain :east}"
       " :domain  :east}"]

      ["{:preview {:bytes 10,"
       "           :group :ops},"
       " :records {:bytes 20,"
       "          :group :dev}}"]
      ["{:preview {:bytes 10,"
       "           :group :ops},"
       " :records {:bytes 20,"
       "          :group  :dev}}"]

      ["{:db 1"
       ""
       " :memory-pressure 2"
       " :ttl 3}"]
      ["{:db 1"
       ""
       " :memory-pressure 2"
       " :ttl             3}"]

      ["#:acct{:a 1"
       " :very-long-key 2}"]
      ["#:acct{:a       1"
       " :very-long-key 2}"])))


(deftest clause-skip-count-cases
  (testing "clause forms honor skip counts with defaults and comments"
    (are [source expected]
         (aligned? source expected)

      ["(condp = status"
       "  :ok :done"
       "  ;; keep clause run"
       "  :retryable :again"
       "  :unknown)"]
      ["(condp = status"
       "  :ok        :done"
       "  ;; keep clause run"
       "  :retryable :again"
       "  :unknown)"]

      ["(are [input expected]"
       "  (= (str \"env:\" input) expected)"
       "  :dev \"env:dev\""
       "  :production \"env:production\")"]
      ["(are [input expected]"
       "  (= (str \"env:\" input) expected)"
       "  :dev        \"env:dev\""
       "  :production \"env:production\")"])))


(deftest reader-conditional-cases
  (testing "reader conditionals are aligned by default"
    (are [source expected]
         (aligned? source expected)

      ["#?(:clj runtime-settings"
       "   :cljs browser-settings)"]
      ["#?(:clj  runtime-settings"
       "   :cljs browser-settings)"]

      ["#?@(:clj [runtime-key]"
       "    :cljs [runtime-key])"]
      ["#?@(:clj  [runtime-key]"
       "    :cljs [runtime-key])"])))


(deftest reader-conditional-nested-cases
  (testing "nested map branches inside reader conditionals are aligned"
    (are [source expected]
         (aligned? source expected)

      ["#?(:clj {:id 1"
       "         :service-name 2}"
       "   :cljs {:id 1"
       "          :service-name 2})"]
      ["#?(:clj  {:id           1"
       "          :service-name 2}"
       "   :cljs {:id           1"
       "          :service-name 2})"])))


(deftest comment-boundary-cases
  (testing "line comments do not become alignment columns"
    (are [source expected]
         (aligned? source expected)

      ["{:result 1"
       " ;; keep group boundary"
       " :retries 2}"]
      ["{:result  1"
       " ;; keep group boundary"
       " :retries 2}"])))


(deftest comment-indent-option-cases
  (testing "standalone comments in nested multiline values honor config"
    (are [config source expected]
         (aligned? source expected config)

      {}
      ["{:opts [(str cfg)"
       " ;; keep nested comment with value"
       "        (expand cfg)]}"]
      ["{:opts [(str cfg)"
       "        ;; keep nested comment with value"
       "        (expand cfg)]}"]

      {:indent-comments? false}
      ["{:opts [(str cfg)"
       " ;; keep nested comment with value"
       "        (expand cfg)]}"]
      ["{:opts [(str cfg)"
       " ;; keep nested comment with value"
       "        (expand cfg)]}"])))


(deftest build-demo-core-pipeline-cases
  (testing "demo misaligned fixture is normalized end-to-end"
    (let [rules (-> config/default-config :rules (assoc-in [:align :enabled?] true))
          misaligned (lines ["(let [opts   (-> opts graal-check graal-uberjar)"
                             "      args   [(str (:graal-native-image opts))"
                             "              ;; Verbose output if enabled."
                             "              (when (:verbose opts)"
                             "                  [\"--native-image-info\""
                             "                   \"--verbose\"])"
                             "              ;; Static build flag"
                             "              (when (:graal-static opts)"
                             "                  [\"--libc=musl\""
                             "                 ;; see https://github.com/oracle/graal/issues/3398"
                             "                 \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\""
                             "                 \"--static\"])]"
                             "      result (b/process {:command-args (remove nil? (flatten args))})]"
                             "  result)"])
          canonical (lines ["(let [opts   (-> opts graal-check graal-uberjar)"
                            "      args   [(str (:graal-native-image opts))"
                            "              ;; Verbose output if enabled."
                            "              (when (:verbose opts)"
                            "                [\"--native-image-info\""
                            "                 \"--verbose\"])"
                            "              ;; Static build flag"
                            "              (when (:graal-static opts)"
                            "                [\"--libc=musl\""
                            "                 ;; see https://github.com/oracle/graal/issues/3398"
                            "                 \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\""
                            "                 \"--static\"])]"
                            "      result (b/process {:command-args (remove nil? (flatten args))})]"
                            "  result)"])]
      (is (= canonical (core/reformat-string misaligned rules)))
      (is (= canonical (core/reformat-string canonical rules)))))
  (testing "nested maps in aligned binding vectors keep their own row indentation"
    (let [rules (-> config/default-config :rules (assoc-in [:align :enabled?] true))
          source (lines ["(def versions"
                         "  \"Map of release and snapshot version strings.\""
                         "  (let [major   1"
                         "        minor   1"
                         "        patch   {:release  0"
                         "                 :snapshot \"9999-SNAPSHOT\"}]"
                         "    (update-vals patch #(clojure.string/join \".\" [major minor %]))))"])
          expected (lines ["(def versions"
                           "  \"Map of release and snapshot version strings.\""
                           "  (let [major 1"
                           "        minor 1"
                           "        patch {:release  0"
                           "               :snapshot \"9999-SNAPSHOT\"}]"
                           "    (update-vals patch #(clojure.string/join \".\" [major minor %]))))"])]
      (is (= expected (core/reformat-string source rules)))
      (is (= expected (core/reformat-string expected rules))))))


(deftest opener-line-comment-cases
  (testing "comments immediately after opening delimiters keep original spacing"
    (are [source expected]
         (aligned? source expected)

      ["{;; map note"
       " :a 1"
       " :very-long 2}"]
      ["{;; map note"
       " :a         1"
       " :very-long 2}"]

      ["{ ;; map note"
       " :a 1"
       " :very-long 2}"]
      ["{ ;; map note"
       " :a         1"
       " :very-long 2}"]

      ["#:acct{;; scoped note"
       " :a 1"
       " :very-long 2}"]
      ["#:acct{;; scoped note"
       " :a         1"
       " :very-long 2}"])))


(deftest space-shrinking-cases
  (testing "extra spaces are normalized without merging tokens"
    (are [source expected]
         (aligned? source expected)

      ["{:k          1"
       " :node 2}"]
      ["{:k    1"
       " :node 2}"])))


(deftest custom-form-config-cases
  (testing "custom form names can opt into alignment"
    (are [config source expected]
         (aligned? source expected config)

      {}
      ["(bind-x [k 1"
       "         node 2]"
       "  [k node])"]
      ["(bind-x [k 1"
       "         node 2]"
       "  [k node])"]

      {:extra-binding-forms ["bind-x"]}
      ["(bind-x [k 1"
       "         node 2]"
       "  [k node])"]
      ["(bind-x [k    1"
       "         node 2]"
       "  [k node])"]

      {}
      ["(switchy"
       "  k 1"
       "  node 2)"]
      ["(switchy"
       "  k 1"
       "  node 2)"]

      {:extra-clause-forms {"switchy" 0 "demo/switchy" 0}}
      ["(switchy"
       "  k 1"
       "  node 2)"]
      ["(switchy"
       "  k    1"
       "  node 2)"]

      {:extra-clause-forms {"switchy" 0 "demo/switchy" 0}}
      ["(demo/switchy"
       "  k 1"
       "  node 2)"]
      ["(demo/switchy"
       "  k    1"
       "  node 2)"]))
  (testing "targets can narrow which structures are aligned"
    (are [config source]
         (aligned? source source config)

      {:targets #{:bindings}}
      ["{:ram \"warm\""
       " :region \"dry\"}"]

      {:targets #{:maps}}
      ["(let [ram \"warm\""
       "      region \"dry\"]"
       "  [ram region])"])))


(deftest custom-form-exclusion-precedence-cases
  (testing "excluded forms bypass built-ins and custom form config"
    (are [config source]
         (aligned? source source config)

      {:exclude-forms #{"case"}}
      ["(case action"
       "  \"scan\" (scan/print-usage)"
       "  \"audit\" (audit/print-usage)"
       "  (print-action-usage summary))"]

      {:extra-clause-forms {"switchy" 0}
       :exclude-forms #{"switchy"}}
      ["(switchy"
       "  k 1"
       "  node 2)"])))


(deftest metadata-target-cases
  (testing "alignment still applies to metadata-wrapped maps"
    (are [source expected]
         (aligned? source expected)

      ["^{:doc true} {:k 1"
       "             :name 2}"]
      ["^{:doc true} {:k   1"
       "             :name 2}"]

      ["{:env ^:legacy {:k 1"
       "                 :name 2}"
       " :id 1}"]
      ["{:env ^:legacy {:k     1"
       "                 :name 2}"
       " :id  1}"])))


(deftest comma-heavy-cases
  (testing "commas around aligned forms are preserved"
    (are [source expected]
         (aligned? source expected)

      ["{:k, 1,"
       " :node, 2}"]
      ["{:k,    1,"
       " :node, 2}"]

      ["{:meta \"^\", :meta* \"#^\", :vector \"[\", :map \"{\""
       " :list \"(\", :eval \"#=\", :uneval \"#_\", :fn \"#(\""
       " :set \"#{\", :deref \"@\", :reader-macro \"#\", :unquote \"~\""
       " :var \"#'\", :quote \"'\", :syntax-quote \"`\", :unquote-splicing \"~@\","
       " :namespaced-map \"#\"}"]
      ["{:meta           \"^\",  :meta* \"#^\", :vector       \"[\",  :map              \"{\""
       " :list           \"(\",  :eval  \"#=\", :uneval       \"#_\", :fn               \"#(\""
       " :set            \"#{\", :deref \"@\",  :reader-macro \"#\",  :unquote          \"~\""
       " :var            \"#'\", :quote \"'\",  :syntax-quote \"`\",  :unquote-splicing \"~@\","
       " :namespaced-map \"#\"}"])))


(deftest multiline-pair-layout-cases
  (testing "multi-line key/value rows are left unchanged"
    (are [source]
         (aligned? source source)

      ["{:left"
       " :alpha"
       ""
       " :right"
       " :beta}"]

      ["{:left"
       " ;; keep split layout"
       " :alpha"
       ""
       " :right"
       " :beta}"]

      ["(let [durations (java.util.TreeMap.)"
       "      apply-indent-and-align-rules"
       "      (fn [formatted]"
       "        formatted)])"])))


(deftest non-target-safety-cases
  (testing "forms outside alignment targets remain unchanged"
    (are [source]
         (aligned? source source)

      ["[:id 1"
       " ;; keep vector row comment"
       " :service-name 2]"]
      ["(do"
       "  k 1"
       "  node 2)"]
      ["#{:k 1 :node 2}"])))


(deftest namespaced-map-variant-cases
  (testing "namespaced map variants align value columns"
    (are [source expected]
         (aligned? source expected)

      ["#::{:id 1"
       "    :service-name 2}"]
      ["#::{:id           1"
       "    :service-name 2}"]

      ["^:legacy #::{:id 1"
       "             :service-name 2}"]
      ["^:legacy #::{:id           1"
       "             :service-name 2}"])))


(deftest qualified-clause-form-cases
  (testing "qualified clause forms align like unqualified built-ins"
    (are [source expected]
         (aligned? source expected)

      ["(clojure.core/cond"
       "  done? 1"
       "  session-expired? 2)"]
      ["(clojure.core/cond"
       "  done?            1"
       "  session-expired? 2)"]

      ["(cljs.core/cond-> build"
       "  valid? (emit build)"
       "  critical? (stream build))"]
      ["(cljs.core/cond-> build"
       "  valid?    (emit build)"
       "  critical? (stream build))"])))


(deftest mixed-form-snapshot-cases
  (testing "multiple top-level forms align only when targets are enabled"
    (let [source ["(let [ram \"warm\""
                  "      region \"dry\"]"
                  "  [ram region])"
                  ""
                  "{:id 1"
                  " :service-name 2}"]
          expected ["(let [ram    \"warm\""
                    "      region \"dry\"]"
                    "  [ram region])"
                    ""
                    "{:id           1"
                    " :service-name 2}"]]
      (is (aligned? source expected))
      (is (aligned? source source {:targets #{}})))))


(deftest edge-noop-cases
  (testing "empty and incomplete forms remain unchanged"
    (are [source]
         (aligned? source source)

      ["{}"]
      ["#:acct{}"]
      ["()"]
      ["(condp)"])))


(deftest multiline-continuation-edge-cases
  (testing "multiline value continuations handle trailing and blank lines safely"
    (are [source expected]
         (aligned? source expected)

      ["{:a [1"
       "]"
       " :very-long-key 2}"]
      ["{:a             [1"
       "            ]"
       " :very-long-key 2}"]

      ["{:a [1"
       "   ]"
       " :very-long-key 2}"]
      ["{:a             [1"
       "               ]"
       " :very-long-key 2}"]

      ["{:a [1"
       ",2]"
       " :very-long-key 3}"]
      ["{:a             [1"
       "            ,2]"
       " :very-long-key 3}"]

      ["{:a [a ;; note"
       "     b]"
       " :very-long-key 2}"]
      ["{:a             [a ;; note"
       "                 b]"
       " :very-long-key 2}"]

      ["{:a [\"a"
       "x\" ;; note"
       "     y]"
       " :very-long-key 2}"]
      ["{:a             [\"a"
       "x\" ;; note"
       "                 y]"
       " :very-long-key 2}"]

      ["{:a [(foo x)"
       "  (bar x)]"
       " :very-long-key 2}"]
      ["{:a             [(foo x)"
       "              (bar x)]"
       " :very-long-key 2}"]

      ["{:a [a"
       "     bb"
       "   ccc]"
       " :very-long-key 2}"]
      ["{:a             [a"
       "                 bb"
       "               ccc]"
       " :very-long-key 2}"]

      ["{:a [1"
       " ;; lone comment"
       "]"
       " :very-long-key 2}"]
      ["{:a             [1"
       "             ;; lone comment"
       "            ]"
       " :very-long-key 2}"]))
  (testing "trailing standalone comments keep alignment and closing delimiter spacing"
    (are [source expected]
         (aligned? source expected)

      ["{:a 1"
       " ;; trailing only"
       "}"]
      ["{:a 1"
       " ;; trailing only"
       "}"])))


(deftest clause-inline-comment-continuation-cases
  (testing "clause rows with multiline predicates and inline comments are deterministic"
    (are [source]
         (aligned? source source)

      ["(cond"
       "  \"a"
       "x\" ;; note"
       "  y"
       "  long-name 2)"]

      ["(case kind"
       "  \"a"
       "x\" ;; note"
       "  [v]"
       "  long-name [w]"
       "  :fallback)"])))


(deftest inline-comment-rows
  (testing "a comment's newline ends the current row"
    (are [source expected]
         (aligned? source expected)

      ["{:a 1 ; note"
       " :long 2}"]
      ["{:a    1 ; note"
       " :long 2}"]))
  (testing "a blank line after an inline comment splits alignment groups"
    (are [source expected]
         (aligned? source expected)

      ["{:a 1 ; note"
       ""
       " :long 2"
       " :b 3}"]
      ["{:a 1 ; note"
       ""
       " :long 2"
       " :b    3}"])))


(deftest metadata-cells
  (testing "metadata belongs to the cell without hiding its siblings"
    (are [source expected]
         (aligned? source expected)

      ["{^:tag a 1"
       " longer 2}"]
      ["{^:tag a 1"
       " longer  2}"]

      ["(let [^String a 1"
       "      longer 2] [a longer])"]
      ["(let [^String a 1"
       "      longer    2] [a longer])"]

      ["(let ^:foo [a 1"
       "            longer 2] [a longer])"]
      ["(let ^:foo [a      1"
       "            longer 2] [a longer])"])))


(deftest discarded-head-forms
  (testing "discarded forms do not replace the head or first binding argument"
    (are [source expected]
         (aligned? source expected)

      ["(let #_ignored"
       "  [a 1"
       "   longer 2])"]
      ["(let #_ignored"
       "  [a      1"
       "   longer 2])"]

      ["(#_ignored let"
       "  [a 1"
       "   longer 2])"]
      ["(#_ignored let"
       "  [a      1"
       "   longer 2])"]))
  (testing "clause skip counts count reader forms, leaving discarded text intact"
    (are [source expected]
         (aligned? source expected)

      ["(case #_ignored"
       "  x :a 1"
       "    :long 2)"]
      ["(case #_ignored"
       "  x :a    1"
       "    :long 2)"]

      ["(#_ignored case"
       "  x :a 1"
       "    :long 2)"]
      ["(#_ignored case"
       "  x :a    1"
       "    :long 2)"])))


(deftest standalone-comment-boundaries
  (testing "comments only follow a substantive line in the same blank-line group"
    (are [source expected]
         (aligned? source expected)

      ["{:a [x"
       ";; comment"
       "     y]}"]
      ["{:a [x"
       "     ;; comment"
       "     y]}"]

      ["{:a [x"
       ";; comment"
       ""
       "     y]}"]
      ["{:a [x"
       ";; comment"
       ""
       "     y]}"]

      ["{:a [x"
       ";; comment"
       " "
       "     y]}"]
      ["{:a [x"
       ";; comment"
       " "
       "     y]}"]

      ["{:a [x"
       " ;; comment"
       "y]}"]
      ["{:a [x"
       ";; comment"
       "y]}"])))


(deftest ignored-continuations
  (testing "moving an ignored cell leaves its contents unchanged"
    (are [source expected]
         (aligned? source expected)

      ["{:a ^:cljstyle/ignore (foo"
       "   bar)"
       " :longer 2}"]
      ["{:a      ^:cljstyle/ignore (foo"
       "   bar)"
       " :longer 2}"]

      ["{:a (comment"
       " x"
       "   y)"
       " :longer 2}"]
      ["{:a      (comment"
       " x"
       "   y)"
       " :longer 2}"]))
  (testing "nested ignore barriers also apply during continuation shifts"
    (doseq [ignored [(lines ["^:cljstyle/ignore [x"
                             " y]"])
                     (lines ["(comment"
                             " x"
                             "   y)"])
                     (lines ["#_(foo"
                             " x"
                             "   y)"])]]
      (let [source (str "{:a [before\n     " ignored "\n     after]\n :longer 2}")
            formatted (test-util/apply-formatter align/reformat-form source {})]
        (is (str/includes? formatted ignored) ignored)
        (is (= formatted
               (test-util/apply-formatter align/reformat-form formatted {}))
            ignored)))))


(deftest multiline-literal-idempotence
  (testing "moving a multiline literal does not shift its final physical line"
    (doseq [prefix ["" "#"]]
      (let [first-literal (str prefix "\"foo\nbar\"")
            second-literal (str prefix "\"x\ny\"")
            source (str "{:a " first-literal " :b 3\n :longer " second-literal " :c 6}")
            formatted (test-util/apply-formatter align/reformat-form source {})
            rules (assoc-in (:rules config/default-config) [:align :enabled?] true)
            full-format (core/reformat-string source rules)]
        (is (str/includes? formatted first-literal))
        (is (str/includes? formatted second-literal))
        (is (= formatted
               (test-util/apply-formatter align/reformat-form formatted {})))
        (is (= full-format (core/reformat-string full-format rules)))))))


(deftest nested-dedent-idempotence
  (testing "shrinking outer spacing cannot leave nested columns unstable"
    (doseq [source [(lines ["{:a   {:x 1"
                            " :long-key 2}}"])
                    (lines ["{:a   {:x 1"
                            ":long-key 2}}"])
                    (lines ["{:a   #:a{:x 1"
                            " :long-key 2}}"])]]
      (let [formatted (test-util/apply-formatter align/reformat-form source {})
            rules (-> (:rules config/default-config)
                      (assoc-in [:align :enabled?] true)
                      (assoc-in [:indentation :enabled?] false))
            full-format (core/reformat-string source rules)]
        (is (= formatted
               (test-util/apply-formatter align/reformat-form formatted {}))
            source)
        (is (= full-format (core/reformat-string full-format rules))
            source)))))
