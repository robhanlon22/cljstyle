(ns cljstyle.format.align-test
  (:require
    [cljstyle.format.align :as align]
    [cljstyle.test-util]
    [clojure.test :refer [are deftest testing]]))


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
      "(let [args [(str native-image)\n      ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n      ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)"
      "(let [args [(str native-image)\n            ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n            ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)"

      {:indent-comments? false}
      "(let [args [(str native-image)\n      ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n      ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)"
      "(let [args [(str native-image)\n      ;; Verbose output if enabled.\n            (when verbose?\n              [\"--native-image-info\"\n               \"--verbose\"])\n      ;; Static build flag\n            (when static?\n              [\"--libc=musl\"\n               ;; see issue #3398\n               \"-H:CCompilerOption=-Wl,-z,stack-size=2097152\"\n               \"--static\"])]]\n  args)")))


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
      "(cond\n  x    1\n  node 2)")))


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
      "(cond\n  k,    1,\n  node, 2)")))


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
      "{:left\n ;; keep split layout\n :alpha\n\n :right\n :beta}")))


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

      "{:a [1\n ;; lone comment\n]\n :very-long-key 2}"
      "{:a             [1\n             ;; lone comment\n]\n :very-long-key 2}"))
  (testing "trailing standalone comments keep alignment and closing delimiter spacing"
    (are [source expected]
         (rule-reformatted? align/align-columns {} source expected)

      "{:a 1\n ;; trailing only\n}"
      "{:a 1\n ;; trailing only\n}")))
