(ns cljstyle.format.core-test
  (:require
    [cljstyle.config :as config]
    [cljstyle.format.core :as fmt]
    [cljstyle.format.zloc :as zl]
    [cljstyle.test-util]
    [clojure.string :as str]
    [clojure.test :refer [deftest testing is]]
    [rewrite-clj.parser :as parser]
    [rewrite-clj.zip :as z]))


(def ^:private default-rules
  (:rules config/default-config))


(deftest zprn-debugging
  (let [form (parser/parse-string "(do (prn 123) true)")
        zloc (z/edn* form)]
    (is (str/starts-with?
          (with-out-str
            (is (identical? zloc (zl/zprn zloc :label))))
          ":label "))))


(deftest form-parsing
  (is (reformatted?
        fmt/reformat-form {}
        ";foo"
        ";foo"))
  (is (reformatted?
        fmt/reformat-form {}
        "::foo"
        "::foo"))
  (is (reformatted?
        fmt/reformat-form {}
        "::foo/bar"
        "::foo/bar"))
  (is (reformatted?
        fmt/reformat-form {}
        "foo:bar"
        "foo:bar"))
  (is (reformatted?
        fmt/reformat-form {}
        "[##-Inf 0.0 ##Inf]"
        "[##-Inf 0.0 ##Inf]"))
  (is (reformatted?
        fmt/reformat-form {}
        "#_(foo\nbar)"
        "#_(foo\nbar)"))
  (is (reformatted?
        fmt/reformat-form {}
        "(juxt +' -')"
        "(juxt +' -')"))
  (is (reformatted?
        fmt/reformat-form {}
        "#\"(?i)foo\""
        "#\"(?i)foo\""))
  (is (reformatted?
        fmt/reformat-form {}
        "#\"a\nb\""
        "#\"a\nb\""))
  (is (reformatted?
        fmt/reformat-form {}
        "#?(:clj foo :cljs bar)"
        "#?(:clj foo :cljs bar)")))


(deftest namespaced-maps
  (testing "single namespace"
    (is (reformatted?
          fmt/reformat-form {:whitespace {:insert-missing? false}}
          "#:a{:one 1\n    :two 2}"
          "#:a{:one 1\n    :two 2}"))
    (is (reformatted?
          fmt/reformat-form {:whitespace {:insert-missing? false}}
          "#:a {:one 1\n     :two 2}"
          "#:a {:one 1\n     :two 2}"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "#:a{:one 1\n    :two 2}"
          "#:a{:one 1\n    :two 2}"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "#:a {:one 1\n     :two 2}"
          "#:a {:one 1\n     :two 2}")))
  (testing "let binding"
    (is (reformatted?
          fmt/reformat-form default-rules
          "(let [foo #:a{:one 1}] (:a/one foo))"
          "(let [foo #:a{:one 1}] (:a/one foo))"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "(let [foo #:a {:one 1}] (:a/one foo))"
          "(let [foo #:a {:one 1}] (:a/one foo))"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "(let [foo #:abc\n{:d 1}] (:d foo))"
          "(let [foo #:abc\n          {:d 1}] (:d foo))")))
  (testing "multiline"
    (is (reformatted?
          fmt/reformat-form default-rules
          "#:abc\n {:d 1}"
          "#:abc\n{:d 1}"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "#:abc\n{:d 1}"
          "#:abc\n{:d 1}")))
  (testing "auto-resolved"
    (is (reformatted?
          fmt/reformat-form default-rules
          "#::{ :x 123 }  "
          "#::{:x 123}"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "#::{ :x 123 \n :y true}  "
          "#::{:x 123\n    :y true}"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "#:foo{ :x #::bar{} }  "
          "#:foo{:x #::bar{}}"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "#::foo\n{ :x #::bar {} }  "
          "#::foo\n{:x #::bar {}}"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "#::foo{ :x #::bar{} }  "
          "#::foo{:x #::bar{}}"))))


(deftest metadata-handling
  (is (reformatted?
        fmt/reformat-form default-rules
        "(defonce ^{:doc \"foo\"}\nfoo\n:foo)"
        "(defonce ^{:doc \"foo\"}\n  foo\n  :foo)"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "(def ^:private\nfoo\n:foo)"
        "(def ^:private\n  foo\n  :foo)"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "(def ^:private foo\n:foo)"
        "(def ^:private foo\n  :foo)"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "^\n:a\n:bcd"
        "^:a\n:bcd")))


(deftest qualified-methods
  (is (reformatted?
        fmt/reformat-form default-rules
        "( map Integer/parseInt coll   )"
        "(map Integer/parseInt coll)"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "(let [open ^[File] java.io.FileInputStream/new]\n(open file) \n )"
        "(let [open ^[File] java.io.FileInputStream/new]\n  (open file))"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "(let [   epoch-ms java.time.Instant/.toEpochMilli]\n(mapv epoch-ms times  )\n )"
        "(let [epoch-ms java.time.Instant/.toEpochMilli]\n  (mapv epoch-ms times))")))


(deftest array-classes
  (is (reformatted?
        fmt/reformat-form default-rules
        "(fn [^byte/1 arr]\n(count arr))"
        "(fn [^byte/1 arr]\n  (count arr))"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "(let [grid ^Object/2 arg]\n    (foo grid )  )"
        "(let [grid ^Object/2 arg]\n  (foo grid))")))


(deftest ignored-forms
  (is (reformatted?
        fmt/reformat-form default-rules
        "^:cljstyle/ignore\n(def x\n 123\n  456)"
        "^:cljstyle/ignore\n(def x\n 123\n  456)")))


(deftest fuzzy-matches
  (is (reformatted?
        fmt/reformat-form default-rules
        "(with-foo x\ny\nz)"
        "(with-foo x\n  y\n  z)"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "(defelem foo [x]\n[:foo x])"
        "(defelem foo [x]\n  [:foo x])")))


(deftest aligned-forms
  (let [align-rules (assoc-in default-rules [:align :enabled?] true)
        align-override-rules (assoc-in align-rules [:indentation :indents 'cond->] [[:inner 0]])
        align-numeric-indent-method-rules (-> align-rules
                                              (assoc-in [:indentation :enabled?] false)
                                              (assoc-in [:indentation :indents 'odd-case] [1]))
        align-custom-clause-rules (-> align-rules
                                      (update-in
                                        [:align :extra-clause-forms]
                                        merge
                                        {"switcher" 0
                                         "demo/switcher" 0
                                         "regex-switch" 0})
                                      (update-in
                                        [:indentation :indents]
                                        merge
                                        {'demo/switcher [[:inner 1]]
                                         'switcher [[:inner 1]]
                                         #"^regex-switch$" [[:inner 1]]
                                         :ignore-me [[:inner 0]]}))]
    (is (reformatted?
          fmt/reformat-form default-rules
          "(let [path \"deps\"\n      profile \"tool\"]\n  [path profile])"
          "(let [path \"deps\"\n      profile \"tool\"]\n  [path profile])"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "(let [path \"deps\"\n      profile \"tool\"]\n  [path profile])"
          "(let [path    \"deps\"\n      profile \"tool\"]\n  [path profile])"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "{:path \"deps\"\n :profile \"tool\"}"
          "{:path    \"deps\"\n :profile \"tool\"}"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "(cond-> compile-mode\n  a 1\n  bb 2)"
          "(cond-> compile-mode\n  a  1\n  bb 2)"))
    (is (reformatted?
          fmt/reformat-form align-override-rules
          "(cond-> compile-mode\n  a 1\n  bb 2)"
          "(cond-> compile-mode\n  a                  1\n  bb                 2)"))
    (is (reformatted?
          fmt/reformat-form align-custom-clause-rules
          "(switcher base\n  a 1\n  bb 2)"
          "(switcher base\n          a  1\n          bb 2)"))
    (is (reformatted?
          fmt/reformat-form align-custom-clause-rules
          "(demo/switcher base\n  a 1\n  bb 2)"
          "(demo/switcher base\n               a  1\n               bb 2)"))
    (is (reformatted?
          fmt/reformat-form align-custom-clause-rules
          "(regex-switch base\n  a 1\n  bb 2)"
          "(regex-switch base\n              a  1\n              bb 2)"))
    (is (reformatted?
          fmt/reformat-form align-numeric-indent-method-rules
          "(cond\n  a 1\n  bb 2)"
          "(cond\n  a  1\n  bb 2)"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "{:c [(str base)\n\n     (emit base)]\n :long-key 1}"
          "{:c        [(str base)\n\n            (emit base)]\n :long-key 1}"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "{:a 1 ;; inline\n :long-key 2}"
          "{:a 1 ; inline\n      :long-key 2}"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "{:a 1\n ;; trailing\n}"
          "{:a 1\n ;; trailing\n }"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "#?(:clj :server\n   :cljs :browser)"
          "#?(:clj  :server\n   :cljs :browser)"))
    (is (reformatted?
          fmt/reformat-form align-rules
          "(do ())"
          "(do ())"))))


(deftest eof-newlines
  (is (= ":x" (fmt/reformat-file ":x" (assoc-in default-rules [:eof-newline :enabled?] false))))
  (is (= ":x\n\n\n" (fmt/reformat-file ":x\n\n\n" (assoc-in default-rules [:eof-newline :enabled?] false))))
  (is (= ":x\n" (fmt/reformat-file ":x" default-rules)))
  (is (= ":x\n" (fmt/reformat-file ":x\n" default-rules)))
  (is (= ":x\n" (fmt/reformat-file ":x\n\n\n" default-rules)))
  (is (= ":x\n\n\n" (fmt/reformat-file ":x\n\n\n" (assoc-in default-rules [:eof-newline :trailing-blanks?] true))))
  (is (= ":x\n" (fmt/reformat-file ":x" (assoc-in default-rules [:eof-newline :trailing-blanks?] true))))
  (is (= ":x\n\n\n"
         (fmt/reformat-file
           ":x\n\n\n"
           (-> default-rules
               (assoc-in [:eof-newline :enabled?] false)
               (assoc-in [:eof-newline :trailing-blanks?] false))))))


(deftest comma-placeholders
  (testing "general usage"
    (is (reformatted?
          fmt/reformat-form default-rules
          ",,,"
          ",,,")))
  (testing "collection tails"
    (is (reformatted?
          fmt/reformat-form default-rules
          "(def thing
  [:one  ; a comment
   :two  ; another comment
   ,,,])"
          "(def thing
  [:one  ; a comment
   :two  ; another comment
   ,,,])"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "(def thing
  #{:one 1  ; a comment
    :two 2  ; another comment
    ,,,})"
          "(def thing
  #{:one 1  ; a comment
    :two 2  ; another comment
    ,,,})"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "(def thing
  {:one 1  ; a comment
   :two 2  ; another comment
   ,,,})"
          "(def thing
  {:one 1  ; a comment
   :two 2  ; another comment
   ,,,})"))))


(deftest anonymous-function-syntax
  (is (reformatted?
        fmt/reformat-form default-rules
        "#(while true\n(println :foo))"
        "#(while true\n   (println :foo))"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "#(reify Closeable\n(close [_]\n(prn %)))"
        "#(reify Closeable\n   (close [_]\n     (prn %)))"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "(mapv\n #(vector\n {:foo %\n  :bar 123}\n       %)\nxs)"
        "(mapv\n  #(vector\n     {:foo %\n      :bar 123}\n     %)\n  xs)"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "#(foo\nbar\nbaz)"
        "#(foo\n   bar\n   baz)"))
  (is (reformatted?
        fmt/reformat-form default-rules
        "#(foo bar\nbaz)"
        "#(foo bar\n      baz)"))
  (is (reformatted?
        fmt/reformat-form (assoc-in default-rules [:indentation :indents 'foo] [[:block 1]])
        "#(foo bar\nbaz)"
        "#(foo bar\n   baz)")))


(deftest letfn-indents
  (is (reformatted?
        fmt/reformat-form default-rules
        "(letfn [(f [x]\nx)]\n(let [x (f 1)]\n(str x 2\n3 4)))"
        "(letfn [(f\n          [x]\n          x)]\n  (let [x (f 1)]\n    (str x 2\n         3 4)))"))
  (testing "issue #54"
    (is (reformatted?
          fmt/reformat-form (assoc-in default-rules [:functions :enabled?] false)
          "(letfn [(foo []
          ::foo)
        (bar []
          ::bar)
        (baz []
          ::baz)]
  ::ret)"
          "(letfn [(foo []
          ::foo)
        (bar []
          ::bar)
        (baz []
          ::baz)]
  ::ret)"))
    (is (reformatted?
          fmt/reformat-form default-rules
          "(letfn [(foo []
          ::foo)
        (bar []
          ::bar)
        (baz []
          ::baz)]
  ::ret)"
          "(letfn [(foo
          []
          ::foo)
        (bar
          []
          ::bar)
        (baz
          []
          ::baz)]
  ::ret)"))))


(deftest quoted-forms
  (is (reformatted?
        fmt/reformat-form default-rules
        "(let [x 123
      y 456]
  `(defn ~foo
     ~(str \"foo bar \" x)
     [a# b#]
     ~y))"
        "(let [x 123
      y 456]
  `(defn ~foo
     ~(str \"foo bar \" x)
     [a# b#]
     ~y))")))


(deftest discard-forms
  (is (reformatted?
        fmt/reformat-form default-rules
        "(defn fuga
  [x]
  (+ 1 2)
)

(let [p 1]
  #_(+ 1 2))

(defn hoge [x]
     (+ 1 2)
)"
        "(defn fuga
  [x]
  (+ 1 2))


(let [p 1]
  #_(+ 1 2))


(defn hoge
  [x]
  (+ 1 2))")))


(deftest shebang-lines
  (is (= "#!/usr/local/bin/bb

(println \"scripts are cool\")\n"
         (fmt/reformat-file
           "#!/usr/local/bin/bb

(println \"scripts are cool\")\n"
           default-rules)))
  (is (= "#!/usr/local/bin/clj


(defn foo
  [x]
  (println \"hello, \" x))\n"
         (fmt/reformat-file
           "#!/usr/local/bin/clj


(defn foo [x] (println \"hello, \" x))"
           default-rules))))
