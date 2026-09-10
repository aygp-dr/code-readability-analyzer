(ns code-readability-analyzer.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [code_readability_analyzer.core :as sut]
            [code-readability-analyzer.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
(def ^:private side-effecting
  #{`sut/analyze-file `sut/find-source-files `sut/-main})

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'code_readability_analyzer.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/source-lines ::specs/path-like ::specs/identifier ::specs/line-lengths
             ::specs/metrics ::specs/scores ::specs/result ::specs/results ::specs/cli-spec]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(def ^:private regex? #(instance? java.util.regex.Pattern %))

(deftest real-values-conform
  (testing "lookup tables"
    (is (s/valid? (s/map-of string? ::specs/lang) sut/lang-extensions))
    (is (s/valid? (s/map-of ::specs/lang regex?) sut/function-patterns))
    (is (s/valid? (s/map-of ::specs/lang regex?) sut/line-comment-patterns))
    (is (s/valid? (s/map-of ::specs/lang regex?) sut/branch-keywords))
    (is (s/valid? ::specs/cli-spec sut/cli-spec)))
  (testing "a real analysis of this repo's source"
    (let [result (sut/analyze-file "src/code_readability_analyzer/core.clj")]
      (is (s/valid? ::specs/result result))
      (is (s/valid? ::specs/results [result])))))
