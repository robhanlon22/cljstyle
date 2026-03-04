(ns cljstyle.format.align-test
  (:require
    [cljstyle.config :as config]
    [cljstyle.format.align :as align]
    [cljstyle.format.align.plan :as plan]
    [cljstyle.format.align.spacing :as spacing]
    [cljstyle.format.core :as core]
    [cljstyle.test-util]
    [clojure.test :refer [are deftest is testing]]
    [rewrite-clj.node :as n]
    [rewrite-clj.parser :as parser]
    [rewrite-clj.zip :as z]))


(deftest core-alignment-cases
  (testing "core alignment behavior"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "{:ram \"warm\"\n :region \"dry\"}"
      "{:ram    \"warm\"\n :region \"dry\"}"

      {}
      "(let [ram \"warm\"\n      region \"dry\"]\n  [ram region])"
      "(let [ram    \"warm\"\n      region \"dry\"]\n  [ram region])"

      {}
      "(cond\n  done? 1\n  session-expired? 2)"
      "(cond\n  done?            1\n  session-expired? 2)"

      {}
      "(cond-> build\n  valid? (emit build)\n  critical? (stream build))"
      "(cond-> build\n  valid?    (emit build)\n  critical? (stream build))"

      {}
      "(case action\n  \"scan\" (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))"
      "(case action\n  \"scan\"  (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))"

      {}
      "(case kind\n  :brief 1\n  :larger 22\n  (fallback-with-super-long-name kind))"
      "(case kind\n  :brief  1\n  :larger 22\n  (fallback-with-super-long-name kind))"

      {}
      "{k node\n :customer-records ctx}"
      "{k                 node\n :customer-records ctx}"

      {}
      "(let [k node\n      customer-records ctx])"
      "(let [k                node\n      customer-records ctx])"

      {}
      "{:omega 1\n :x 2}"
      "{:omega 1\n :x     2}"

      {}
      "(cond compile clean ahead\n      ci test run\n      time fix\n      process phase finish)"
      "(cond compile clean ahead\n      ci      test  run\n      time    fix\n      process phase finish)"

      {}
      "{:project {:id :id\n           :domain :east}\n :domain :east}"
      "{:project {:id     :id\n           :domain :east}\n :domain  :east}"

      {}
      "{:preview {:bytes 10,\n           :group :ops},\n :records {:bytes 20,\n          :group :dev}}"
      "{:preview {:bytes 10,\n           :group :ops},\n :records {:bytes 20,\n          :group  :dev}}"

      {}
      "{:db 1\n\n :memory-pressure 2\n :ttl 3}"
      "{:db 1\n\n :memory-pressure 2\n :ttl             3}"

      {}
      "#:acct{:a 1\n :very-long-key 2}"
      "#:acct{:a       1\n :very-long-key 2}")))


(deftest clause-skip-count-cases
  (testing "clause forms honor skip counts with defaults and comments"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "(condp = status\n  :ok :done\n  ;; keep clause run\n  :retryable :again\n  :unknown)"
      "(condp = status\n  :ok        :done\n  ;; keep clause run\n  :retryable :again\n  :unknown)"

      {}
      "(are [input expected]\n  (= (str \"env:\" input) expected)\n  :dev \"env:dev\"\n  :production \"env:production\")"
      "(are [input expected]\n  (= (str \"env:\" input) expected)\n  :dev        \"env:dev\"\n  :production \"env:production\")")))


(deftest reader-conditional-cases
  (testing "reader conditionals are aligned by default"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "#?(:clj runtime-settings\n   :cljs browser-settings)"
      "#?(:clj  runtime-settings\n   :cljs browser-settings)"

      {}
      "#?@(:clj [runtime-key]\n    :cljs [runtime-key])"
      "#?@(:clj  [runtime-key]\n    :cljs [runtime-key])")))


(deftest reader-conditional-nested-cases
  (testing "nested map branches inside reader conditionals are aligned"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "#?(:clj {:id 1\n         :service-name 2}\n   :cljs {:id 1\n          :service-name 2})"
      "#?(:clj  {:id           1\n          :service-name 2}\n   :cljs {:id           1\n          :service-name 2})")))


(deftest comment-boundary-cases
  (testing "line comments do not become alignment columns"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "(let [result 1\n      ;; keep group boundary\n      retries 1]\n  retries)"
      "(let [result  1\n      ;; keep group boundary\n      retries 1]\n  retries)"

      {}
      "{:result 1\n ;; keep group boundary\n :retries 2}"
      "{:result  1\n ;; keep group boundary\n :retries 2}"

      {}
      "(case phase\n  :ok 1\n  ;; keep group boundary\n  :again 2)"
      "(case phase\n  :ok    1\n  ;; keep group boundary\n  :again 2)")))


(deftest comment-indent-option-cases
  (testing "standalone comments in nested multiline values honor config"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "(let [opts (normalize opts)\n      args [(str opts)\n      ;; Verbose output if enabled.\n            (when verbose?\n              \"--verbose\")]]\n  args)"
      "(let [opts (normalize opts)\n      args [(str opts)\n            ;; Verbose output if enabled.\n            (when verbose?\n              \"--verbose\")]]\n  args)"

      {:indent-comments? false}
      "(let [opts (normalize opts)\n      args [(str opts)\n      ;; Verbose output if enabled.\n            (when verbose?\n              \"--verbose\")]]\n  args)"
      "(let [opts (normalize opts)\n      args [(str opts)\n      ;; Verbose output if enabled.\n            (when verbose?\n              \"--verbose\")]]\n  args)"

      {}
      "{:opts [(str cfg)\n ;; keep nested comment with value\n        (expand cfg)]}"
      "{:opts [(str cfg)\n        ;; keep nested comment with value\n        (expand cfg)]}"

      {:indent-comments? false}
      "{:opts [(str cfg)\n ;; keep nested comment with value\n        (expand cfg)]}"
      "{:opts [(str cfg)\n ;; keep nested comment with value\n        (expand cfg)]}"))
  (testing "build-style multiline vectors keep comments aligned with following forms"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "(let [opts (-> opts graal-check graal-uberjar)\n      args [(str (:graal-native-image opts))\n            ;; Verbose output if enabled.\n            (when (:verbose opts)\n              [\"--native-image-info\"\n               \"--verbose\"])\n            ;; Static build flag\n            (when (:graal-static opts)\n              [\"--libc=musl\"\n               ;; see https://github.com/oracle/graal/issues/3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]\n      result (b/process {:command-args (remove nil? (flatten args))})]\n  result)"
      "(let [opts   (-> opts graal-check graal-uberjar)\n      args   [(str (:graal-native-image opts))\n              ;; Verbose output if enabled.\n              (when (:verbose opts)\n                [\"--native-image-info\"\n                 \"--verbose\"])\n              ;; Static build flag\n              (when (:graal-static opts)\n                [\"--libc=musl\"\n                 ;; see https://github.com/oracle/graal/issues/3398\n                 \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n                 \"--static\"])]\n      result (b/process {:command-args (remove nil? (flatten args))})]\n  result)"

      {}
      "(let [args [(str native-image)\n      ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n      ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)"
      "(let [args [(str native-image)\n            ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n            ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)"

      {:indent-comments? false}
      "(let [args [(str native-image)\n      ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n      ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)"
      "(let [args [(str native-image)\n      ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n      ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)")))


(deftest build-demo-core-pipeline-cases
  (testing "demo misaligned fixture is normalized end-to-end"
    (let [rules (-> config/default-config :rules (assoc-in [:align :enabled?] true))
          misaligned "(let [opts   (-> opts graal-check graal-uberjar)\n      args   [(str (:graal-native-image opts))\n              ;; Verbose output if enabled.\n              (when (:verbose opts)\n                  [\"--native-image-info\"\n                   \"--verbose\"])\n              ;; Static build flag\n              (when (:graal-static opts)\n                  [\"--libc=musl\"\n                 ;; see https://github.com/oracle/graal/issues/3398\n                 \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n                 \"--static\"])]\n      result (b/process {:command-args (remove nil? (flatten args))})]\n  result)"
          canonical "(let [opts   (-> opts graal-check graal-uberjar)\n      args   [(str (:graal-native-image opts))\n              ;; Verbose output if enabled.\n              (when (:verbose opts)\n                [\"--native-image-info\"\n                 \"--verbose\"])\n              ;; Static build flag\n              (when (:graal-static opts)\n                [\"--libc=musl\"\n                 ;; see https://github.com/oracle/graal/issues/3398\n                 \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n                 \"--static\"])]\n      result (b/process {:command-args (remove nil? (flatten args))})]\n  result)"]
      (is (= canonical (core/reformat-string misaligned rules)))
      (is (= canonical (core/reformat-string canonical rules)))))
  (testing "nested maps in aligned binding vectors keep their own row indentation"
    (let [rules (-> config/default-config :rules (assoc-in [:align :enabled?] true))
          source "(def versions\n  \"Map of release and snapshot version strings.\"\n  (let [major   1\n        minor   1\n        patch   {:release  0\n                 :snapshot \"9999-SNAPSHOT\"}]\n    (update-vals patch #(clojure.string/join \".\" [major minor %]))))"
          expected "(def versions\n  \"Map of release and snapshot version strings.\"\n  (let [major 1\n        minor 1\n        patch {:release  0\n               :snapshot \"9999-SNAPSHOT\"}]\n    (update-vals patch #(clojure.string/join \".\" [major minor %]))))"]
      (is (= expected (core/reformat-string source rules)))
      (is (= expected (core/reformat-string expected rules))))))


(deftest opener-line-comment-cases
  (testing "comments immediately after opening delimiters keep original spacing"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "{;; map note\n :a 1\n :very-long 2}"
      "{;; map note\n :a         1\n :very-long 2}"

      "{ ;; map note\n :a 1\n :very-long 2}"
      "{ ;; map note\n :a         1\n :very-long 2}"

      "#:acct{;; scoped note\n :a 1\n :very-long 2}"
      "#:acct{;; scoped note\n :a         1\n :very-long 2}")))


(deftest idempotence-cases
  (testing "first pass reaches canonical form"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "{:ram \"warm\"\n :region \"dry\"}"
      "{:ram    \"warm\"\n :region \"dry\"}"

      {}
      "(let [p 1\n      node 2]\n  [p node])"
      "(let [p    1\n      node 2]\n  [p node])"

      {}
      "(cond\n  x 1\n  node 2)"
      "(cond\n  x    1\n  node 2)"))
  (testing "second pass keeps canonical form unchanged"
    (are [config formatted]
         (rule-reformatted? align/align-columns config formatted formatted)

      {}
      "{:ram    \"warm\"\n :region \"dry\"}"

      {}
      "(let [p    1\n      node 2]\n  [p node])"

      {}
      "(cond\n  x    1\n  node 2)"

      {}
      "(let [opts   (-> opts graal-check graal-uberjar)\n      args   [(str (:graal-native-image opts))\n              ;; Verbose output if enabled.\n              (when (:verbose opts)\n                [\"--native-image-info\"\n                 \"--verbose\"])\n              ;; Static build flag\n              (when (:graal-static opts)\n                [\"--libc=musl\"\n                 ;; see https://github.com/oracle/graal/issues/3398\n                 \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n                 \"--static\"])]\n      result (b/process {:command-args (remove nil? (flatten args))})]\n  result)")))


(deftest space-shrinking-cases
  (testing "extra spaces are normalized without merging tokens"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "{:k          1\n :node 2}"
      "{:k    1\n :node 2}"

      "(let [k          1\n      node 2]\n  [k node])"
      "(let [k    1\n      node 2]\n  [k node])"

      "(cond\n  k          1\n  node 2)"
      "(cond\n  k    1\n  node 2)")))


(deftest custom-form-config-cases
  (testing "custom form names can opt into alignment"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {}
      "(bind-x [k 1\n         node 2]\n  [k node])"
      "(bind-x [k 1\n         node 2]\n  [k node])"

      {:extra-binding-forms ["bind-x"]}
      "(bind-x [k 1\n         node 2]\n  [k node])"
      "(bind-x [k    1\n         node 2]\n  [k node])"

      {}
      "(switchy\n  k 1\n  node 2)"
      "(switchy\n  k 1\n  node 2)"

      {:extra-clause-forms {"switchy" 0 "demo/switchy" 0}}
      "(switchy\n  k 1\n  node 2)"
      "(switchy\n  k    1\n  node 2)"

      {:extra-clause-forms {"switchy" 0 "demo/switchy" 0}}
      "(demo/switchy\n  k 1\n  node 2)"
      "(demo/switchy\n  k    1\n  node 2)"))
  (testing "targets can narrow which structures are aligned"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {:targets #{:bindings}}
      "{:ram \"warm\"\n :region \"dry\"}"
      "{:ram \"warm\"\n :region \"dry\"}"

      {:targets #{:maps}}
      "(let [ram \"warm\"\n      region \"dry\"]\n  [ram region])"
      "(let [ram \"warm\"\n      region \"dry\"]\n  [ram region])")))


(deftest custom-form-exclusion-precedence-cases
  (testing "excluded forms bypass built-ins and custom form config"
    (are [config source expected]
         (rule-reformatted? align/align-columns config source expected)

      {:exclude-forms #{"case"}}
      "(case action\n  \"scan\" (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))"
      "(case action\n  \"scan\" (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))"

      {:extra-clause-forms {"switchy" 0}
       :exclude-forms #{"switchy"}}
      "(switchy\n  k 1\n  node 2)"
      "(switchy\n  k 1\n  node 2)")))


(deftest metadata-target-cases
  (testing "alignment still applies to metadata-wrapped maps"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "^{:doc true} {:k 1\n             :name 2}"
      "^{:doc true} {:k   1\n             :name 2}"

      "{:env ^:legacy {:k 1\n                 :name 2}\n :id 1}"
      "{:env ^:legacy {:k     1\n                 :name 2}\n :id  1}")))


(deftest comma-heavy-cases
  (testing "commas around aligned forms are preserved"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "{:k, 1,\n :node, 2}"
      "{:k,    1,\n :node, 2}"

      "(let [k, 1,\n      node, 2]\n  [k node])"
      "(let [k,    1,\n      node, 2]\n  [k node])"

      "(cond\n  k, 1,\n  node, 2)"
      "(cond\n  k,    1,\n  node, 2)"

      "{:meta \"^\", :meta* \"#^\", :vector \"[\", :map \"{\"\n :list \"(\", :eval \"#=\", :uneval \"#_\", :fn \"#(\"\n :set \"#{\", :deref \"@\", :reader-macro \"#\", :unquote \"~\"\n :var \"#'\", :quote \"'\", :syntax-quote \"`\", :unquote-splicing \"~@\",\n :namespaced-map \"#\"}"
      "{:meta           \"^\",  :meta* \"#^\", :vector       \"[\",  :map              \"{\"\n :list           \"(\",  :eval  \"#=\", :uneval       \"#_\", :fn               \"#(\"\n :set            \"#{\", :deref \"@\",  :reader-macro \"#\",  :unquote          \"~\"\n :var            \"#'\", :quote \"'\",  :syntax-quote \"`\",  :unquote-splicing \"~@\",\n :namespaced-map \"#\"}")))


(deftest multiline-pair-layout-cases
  (testing "multi-line key/value rows are left unchanged"
    (are [config source]
         (rule-reformatted? align/align-columns config source source)

      {}
      "{:left\n :alpha\n\n :right\n :beta}"

      {}
      "(let [left\n      :alpha\n\n      right\n      :beta])"

      {}
      "{:source\n 1\n\n :very-verbose-symbol\n 2\n\n :id\n 3}"

      {}
      "(let [source\n      1\n\n      very-verbose-symbol\n      2\n\n      id\n      3])"

      {}
      "{:left\n ;; keep split layout\n :alpha\n\n :right\n :beta}"

      {}
      "(let [durations (java.util.TreeMap.)\n      apply-indent-and-align-rules\n      (fn [formatted]\n        formatted)])")))


(deftest non-target-safety-cases
  (testing "forms outside alignment targets remain unchanged"
    (are [source]
         (rule-reformatted? align/align-columns {} source source)

      "[:k 1\n :node 2]"
      "(do\n  k 1\n  node 2)"
      "#{:k 1 :node 2}")))


(deftest namespaced-map-variant-cases
  (testing "namespaced map variants align value columns"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "#::{:id 1\n    :service-name 2}"
      "#::{:id           1\n    :service-name 2}"

      "^:legacy #::{:id 1\n             :service-name 2}"
      "^:legacy #::{:id           1\n             :service-name 2}")))


(deftest qualified-clause-form-cases
  (testing "qualified clause forms align like unqualified built-ins"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "(clojure.core/cond\n  done? 1\n  session-expired? 2)"
      "(clojure.core/cond\n  done?            1\n  session-expired? 2)"

      "(clojure.core/case action\n  \"scan\" (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))"
      "(clojure.core/case action\n  \"scan\"  (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))"

      "(cljs.core/cond-> build\n  valid? (emit build)\n  critical? (stream build))"
      "(cljs.core/cond-> build\n  valid?    (emit build)\n  critical? (stream build))")))


(deftest mixed-form-snapshot-cases
  (testing "mixed forms align together when targets are enabled"
    (let [source "(let [ram \"warm\"\n      region \"dry\"]\n  [ram region])\n\n{:id 1\n :service-name 2}\n\n(clojure.core/case action\n  \"scan\" (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))\n\n#?(:clj runtime-settings\n   :cljs browser-settings)"]
      (are [config expected]
           (rule-reformatted? align/align-columns config source expected)

        {}
        "(let [ram    \"warm\"\n      region \"dry\"]\n  [ram region])\n\n{:id           1\n :service-name 2}\n\n(clojure.core/case action\n  \"scan\"  (scan/print-usage)\n  \"audit\" (audit/print-usage)\n  (print-action-usage summary))\n\n#?(:clj  runtime-settings\n   :cljs browser-settings)"

        {:targets #{}}
        source))))


(deftest non-target-lookalike-cases
  (testing "pair-like vectors remain unchanged"
    (are [source]
         (rule-reformatted? align/align-columns {} source source)

      "[:id 1\n :service-name 2]"
      "[:id 1\n ;; keep vector row comment\n :service-name 2]"
      "[:id 1\n\n :service-name 2]")))


(deftest edge-noop-cases
  (testing "edge shapes stay stable while still exercising align walks"
    (are [config source]
         (rule-reformatted? align/align-columns config source source)

      {}
      "{}"

      {}
      "#:acct{}"

      {}
      "()"

      {}
      "(condp)"

      {}
      "(let\n  [aa, 1\n   ;; keep line\n   bb, 2])"

      {:indent-comments? false}
      "{:a 1\n ;; keep standalone\n :b 2}"

      {}
      "{:a 1\n ;; trailing note\n}")))


(deftest multiline-continuation-edge-cases
  (testing "multiline value continuations handle trailing and blank lines safely"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "{:a [1\n]\n :very-long-key 2}"
      "{:a             [1\n]\n :very-long-key 2}"

      "{:a [1\n   ]\n :very-long-key 2}"
      "{:a             [1\n               ]\n :very-long-key 2}"

      "{:a [\n]\n :very-long-key 2}"
      "{:a             [\n]\n :very-long-key 2}"

      "{:a [1\n,2]\n :very-long-key 3}"
      "{:a             [1\n            ,2]\n :very-long-key 3}"

      "{:a [a ;; note\n     b]\n :very-long-key 2}"
      "{:a             [a ;; note\n                 b]\n :very-long-key 2}"

      "{:a [\"a\nx\" ;; note\n     y]\n :very-long-key 2}"
      "{:a             [\"a\nx\"               ;; note\n                 y]\n :very-long-key 2}"

      "{:a [(foo x)\n  (bar x)]\n :very-long-key 2}"
      "{:a             [(foo x)\n              (bar x)]\n :very-long-key 2}"

      "{:a [(foo x)\n     (bar x)]\n :very-long-key 2}"
      "{:a             [(foo x)\n                 (bar x)]\n :very-long-key 2}"

      "{:a [a\n     bb\n   ccc]\n :very-long-key 2}"
      "{:a             [a\n                 bb\n                 ccc]\n :very-long-key 2}"

      "{:a [1\n ;; lone comment\n]\n :very-long-key 2}"
      "{:a             [1\n             ;; lone comment\n]\n :very-long-key 2}"))
  (testing "trailing standalone comments keep alignment and closing delimiter spacing"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "{:a 1\n ;; trailing only\n}"
      "{:a 1\n ;; trailing only\n}")))


(deftest clause-inline-comment-continuation-cases
  (testing "clause rows with multiline predicates and inline comments are deterministic"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "(cond\n  \"a\nx\" ;; note\n  y\n  long-name 2)"
      "(cond\n  \"a\nx\" ;; note\n            y\n  long-name 2)"

      "(case kind\n  \"a\nx\" ;; note\n  [v]\n  long-name [w]\n  :fallback)"
      "(case kind\n  \"a\nx\" ;; note\n            [v]\n  long-name [w]\n  :fallback)")))


(deftest planner-defensive-column-gap-cases
  (testing "planner skips unconstrained destination columns and continues"
    (is (= {[:g 2] 3}
           (plan/plan-column-targets
             {:cells {:src {:row-id :row-1
                            :end 2}
                      :dst {:row-id :row-1
                            :start 4
                            :has-left-space? true
                            :left-space-width 1}}
              :cells-by-column {[:g 2] [:dst]}
              :constraints {[:g 2] [:src]}
              :max-column-by-group {:g 2}})))))


(deftest spacing-edge-cases
  (testing "line-start nodes without left whitespace ignore negative spacing deltas"
    (let [root (z/edn* (parser/parse-string-all "[a\nb]") {:track-position? true})
          line-start (-> root z/down z/down z/right* z/right*)]
      (is (= "[a\nb]"
             (-> (spacing/adjust-left-spacing line-start -1)
                 z/root
                 n/string))))))
