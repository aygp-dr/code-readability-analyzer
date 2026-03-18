(ns code_readability_analyzer.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [code_readability_analyzer.core :as core]))

;; --- Language detection ---

(deftest detect-language-test
  (testing "Detects languages by extension"
    (is (= :python (core/detect-language "foo.py")))
    (is (= :javascript (core/detect-language "app.js")))
    (is (= :javascript (core/detect-language "app.tsx")))
    (is (= :clojure (core/detect-language "core.clj")))
    (is (= :java (core/detect-language "Main.java")))
    (is (= :go (core/detect-language "main.go")))
    (is (= :rust (core/detect-language "lib.rs")))
    (is (= :unknown (core/detect-language "readme.md")))))

;; --- Line length metrics ---

(deftest line-length-metrics-test
  (testing "Empty input"
    (let [m (core/line-length-metrics [])]
      (is (= 0 (:avg-line-length m)))
      (is (= 0 (:max-line-length m)))))
  (testing "Normal lines"
    (let [m (core/line-length-metrics ["abc" "abcdef"])]
      (is (= 4.5 (:avg-line-length m)))
      (is (= 6 (:max-line-length m))))))

;; --- Nesting depth ---

(deftest max-nesting-depth-test
  (testing "No nesting"
    (is (= 0 (core/max-nesting-depth ["x = 1" "y = 2"]))))
  (testing "Simple nesting"
    (is (= 2 (core/max-nesting-depth ["if (x) {" "  if (y) {" "  }" "}"]))))
  (testing "Deep nesting"
    (is (= 3 (core/max-nesting-depth ["(((" ")))"])))))

;; --- Naming consistency ---

(deftest naming-consistency-test
  (testing "All snake_case"
    (is (> (core/naming-consistency ["my_var = 1" "other_var = 2"]) 0.9)))
  (testing "All camelCase"
    (is (> (core/naming-consistency ["myVar = 1" "otherVar = 2"]) 0.9)))
  (testing "Mixed naming"
    (is (< (core/naming-consistency ["myVar = 1" "other_var = 2"]) 0.8))))

;; --- Comment ratio ---

(deftest comment-ratio-test
  (testing "No comments"
    (is (= 0.0 (core/comment-ratio ["x = 1" "y = 2"] :python))))
  (testing "All comments"
    (is (= 1.0 (core/comment-ratio ["# comment" "# another"] :python))))
  (testing "50% comments"
    (is (= 0.5 (core/comment-ratio ["# comment" "x = 1"] :python)))))

;; --- Cyclomatic complexity proxy ---

(deftest cyclomatic-complexity-test
  (testing "No branches"
    (is (= 0.0 (core/cyclomatic-complexity-proxy ["x = 1" "y = 2"] :python))))
  (testing "Some branches"
    (is (> (core/cyclomatic-complexity-proxy
            ["if x > 0:" "  y = 1" "elif x < 0:" "  y = -1" "else:" "  y = 0"]
            :python)
           0.0))))

;; --- Scoring functions ---

(deftest score-avg-line-length-test
  (testing "Short lines get full score"
    (is (= 15.0 (core/score-avg-line-length 60))))
  (testing "Very long lines get zero"
    (is (= 0.0 (core/score-avg-line-length 120))))
  (testing "Borderline"
    (is (= 15.0 (core/score-avg-line-length 80)))))

(deftest score-max-line-length-test
  (testing "Within limit"
    (is (= 10.0 (core/score-max-line-length 100))))
  (testing "Over limit"
    (is (= 0.0 (core/score-max-line-length 200)))))

(deftest score-nesting-depth-test
  (testing "Low nesting"
    (is (= 15.0 (core/score-nesting-depth 3))))
  (testing "High nesting"
    (is (= 0.0 (core/score-nesting-depth 10)))))

(deftest score-naming-consistency-test
  (testing "Perfect consistency"
    (is (= 15.0 (core/score-naming-consistency 1.0))))
  (testing "No consistency"
    (is (= 0.0 (core/score-naming-consistency 0.0)))))

;; --- Scoring range ---

(deftest score-comment-ratio-test
  (testing "Ideal range"
    (is (= 15.0 (core/score-comment-ratio 0.15))))
  (testing "No comments"
    (is (= 0.0 (core/score-comment-ratio 0.0))))
  (testing "Too many comments"
    (is (= 0.0 (core/score-comment-ratio 0.50)))))

(deftest score-complexity-test
  (testing "Low complexity"
    (is (= 15.0 (core/score-complexity 0.05))))
  (testing "High complexity"
    (is (= 0.0 (core/score-complexity 0.4)))))

;; --- Integration: analyze a file ---

(deftest analyze-file-test
  (testing "Analyze this test file"
    (let [result (core/analyze-file "test/code_readability_analyzer/core_test.clj")]
      (is (= "clojure" (:language result)))
      (is (number? (:score result)))
      (is (<= 0 (:score result) 100))
      (is (pos? (:lines result)))
      (is (contains? (:metrics result) :avg-line-length))
      (is (contains? (:metrics result) :max-line-length))
      (is (contains? (:metrics result) :avg-function-length))
      (is (contains? (:metrics result) :max-nesting-depth))
      (is (contains? (:metrics result) :naming-consistency))
      (is (contains? (:metrics result) :comment-ratio))
      (is (contains? (:metrics result) :cyclomatic-complexity-proxy)))))

(deftest analyze-core-file-test
  (testing "Analyze the core source file"
    (let [result (core/analyze-file "src/code_readability_analyzer/core.clj")]
      (is (= "clojure" (:language result)))
      (is (<= 0 (:score result) 100)))))

;; --- File discovery ---

(deftest find-source-files-test
  (testing "Finds source files in src/"
    (let [files (core/find-source-files "src")]
      (is (seq files))
      (is (some #(str/ends-with? (str %) "core.clj") files)))))

;; --- Output formatting ---

(deftest format-text-test
  (testing "Text format includes score"
    (let [result (core/analyze-file "src/code_readability_analyzer/core.clj")
          text (core/format-text [result])]
      (is (str/includes? text "Score:"))
      (is (str/includes? text "/100")))))

(deftest format-json-test
  (testing "JSON format is valid"
    (let [result (core/analyze-file "src/code_readability_analyzer/core.clj")
          json-str (core/format-json [result])
          parsed (cheshire.core/parse-string json-str true)]
      (is (= 1 (get-in parsed [:summary :file-count])))
      (is (number? (get-in parsed [:summary :average-score]))))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
