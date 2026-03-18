(ns code_readability_analyzer.core
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [cheshire.core :as json]))

;; --- Language detection ---

(def lang-extensions
  {"clj" :clojure "cljs" :clojure "cljc" :clojure
   "py" :python
   "js" :javascript "jsx" :javascript "ts" :javascript "tsx" :javascript
   "java" :java
   "c" :c "h" :c "cpp" :cpp "hpp" :cpp "cc" :cpp
   "rb" :ruby
   "go" :go
   "rs" :rust
   "sh" :shell "bash" :shell "zsh" :shell})

(defn detect-language [filepath]
  (let [ext (some-> (fs/extension filepath) str/lower-case)]
    (get lang-extensions ext :unknown)))

;; --- Metric: line lengths ---

(defn line-length-metrics [lines]
  (if (empty? lines)
    {:avg-line-length 0 :max-line-length 0}
    (let [lengths (mapv count lines)]
      {:avg-line-length (double (/ (reduce + lengths) (count lengths)))
       :max-line-length (apply max lengths)})))

;; --- Metric: function length ---

(def function-patterns
  {:python    #"^\s*def\s+"
   :javascript #"(?:function\s+\w+|(?:const|let|var)\s+\w+\s*=\s*(?:async\s+)?(?:function|\([^)]*\)\s*=>|\w+\s*=>))"
   :java      #"(?:public|private|protected|static|\s)+[\w<>\[\]]+\s+\w+\s*\("
   :clojure   #"^\s*\(defn?\s+"
   :c         #"^[\w*]+\s+\w+\s*\([^;]*$"
   :cpp       #"^[\w*:]+\s+\w+\s*\([^;]*$"
   :ruby      #"^\s*def\s+"
   :go        #"^func\s+"
   :rust      #"^\s*(?:pub\s+)?fn\s+"
   :shell     #"^\s*\w+\s*\(\)\s*\{|^\s*function\s+"})

(defn find-function-starts [lines lang]
  (let [pat (get function-patterns lang)]
    (if pat
      (keep-indexed (fn [i line] (when (re-find pat line) i)) lines)
      [])))

(defn avg-function-length [lines lang]
  (let [starts (vec (find-function-starts lines lang))]
    (if (< (count starts) 2)
      ;; With 0-1 functions, estimate from total lines
      (if (empty? starts)
        (count lines)
        (- (count lines) (first starts)))
      ;; Average gap between consecutive function starts
      (let [gaps (map (fn [a b] (- b a)) starts (rest starts))
            last-gap (- (count lines) (last starts))]
        (double (/ (reduce + (conj (vec gaps) last-gap))
                   (count starts)))))))

;; --- Metric: nesting depth ---

(def nesting-openers #{\{ \( \[})
(def nesting-closers #{\} \) \]})

(defn max-nesting-depth [lines]
  (loop [depth 0
         max-depth 0
         remaining (apply str (interpose "\n" lines))]
    (if (empty? remaining)
      max-depth
      (let [ch (first remaining)]
        (cond
          (nesting-openers ch)
          (recur (inc depth) (max max-depth (inc depth)) (rest remaining))

          (nesting-closers ch)
          (recur (max 0 (dec depth)) max-depth (rest remaining))

          :else
          (recur depth max-depth (rest remaining)))))))

;; --- Metric: naming consistency ---

(def identifier-pattern #"\b([a-z][a-zA-Z0-9_]*)\b")

(defn classify-name [name-str]
  (cond
    (re-find #"_" name-str) (if (re-find #"[A-Z]" name-str) :mixed :snake_case)
    (re-find #"[A-Z]" name-str) :camelCase
    :else :neutral))

(defn naming-consistency [lines]
  (let [all-text (str/join "\n" lines)
        names (->> (re-seq identifier-pattern all-text)
                   (map first)
                   (filter #(> (count %) 3))
                   (map classify-name)
                   (remove #{:neutral}))
        freqs (frequencies names)
        total (reduce + 0 (vals freqs))]
    (if (zero? total)
      1.0
      (let [dominant (apply max (vals freqs))]
        (double (/ dominant total))))))

;; --- Metric: comment ratio ---

(def line-comment-patterns
  {:python     #"^\s*#"
   :javascript #"^\s*//"
   :java       #"^\s*//"
   :clojure    #"^\s*;"
   :c          #"^\s*//"
   :cpp        #"^\s*//"
   :ruby       #"^\s*#"
   :go         #"^\s*//"
   :rust       #"^\s*//"
   :shell      #"^\s*#"
   :unknown    #"^\s*(?://|#|;)"})

(defn comment-ratio [lines lang]
  (let [pat (get line-comment-patterns lang (get line-comment-patterns :unknown))
        non-blank (remove str/blank? lines)
        total (count non-blank)]
    (if (zero? total)
      0.0
      (let [comment-count (count (filter #(re-find pat %) non-blank))]
        (double (/ comment-count total))))))

;; --- Metric: cyclomatic complexity proxy ---

(def branch-keywords
  {:python     #"\b(if|elif|else|for|while|except|and|or)\b"
   :javascript #"\b(if|else|for|while|switch|case|catch|\?\?|&&|\|\|)\b"
   :java       #"\b(if|else|for|while|switch|case|catch|&&|\|\|)\b"
   :clojure    #"\b(if|when|cond|case|or|and|loop)\b"
   :c          #"\b(if|else|for|while|switch|case|&&|\|\|)\b"
   :cpp        #"\b(if|else|for|while|switch|case|catch|&&|\|\|)\b"
   :ruby       #"\b(if|elsif|else|unless|for|while|until|case|when|rescue|and|or)\b"
   :go         #"\b(if|else|for|switch|case|select|&&|\|\|)\b"
   :rust       #"\b(if|else|for|while|loop|match|&&|\|\|)\b"
   :shell      #"\b(if|elif|else|for|while|until|case)\b"
   :unknown    #"\b(if|else|for|while|switch|case)\b"})

(defn cyclomatic-complexity-proxy [lines lang]
  (let [pat (get branch-keywords lang (get branch-keywords :unknown))
        total-lines (count (remove str/blank? lines))]
    (if (zero? total-lines)
      0.0
      (let [branch-count (reduce + 0 (map #(count (re-seq pat %)) lines))]
        (double (/ branch-count total-lines))))))

;; --- Scoring ---

(defn score-avg-line-length
  "Ideal: <= 80. Penalty ramps from 80 to 120."
  [avg]
  (cond
    (<= avg 80) 15.0
    (>= avg 120) 0.0
    :else (* 15.0 (/ (- 120 avg) 40.0))))

(defn score-max-line-length
  "Ideal: <= 120. Penalty ramps from 120 to 200."
  [mx]
  (cond
    (<= mx 120) 10.0
    (>= mx 200) 0.0
    :else (* 10.0 (/ (- 200 mx) 80.0))))

(defn score-avg-function-length
  "Ideal: <= 20 lines. Penalty ramps from 20 to 60."
  [avg]
  (cond
    (<= avg 20) 15.0
    (>= avg 60) 0.0
    :else (* 15.0 (/ (- 60 avg) 40.0))))

(defn score-nesting-depth
  "Ideal: <= 4. Penalty ramps from 4 to 10."
  [depth]
  (cond
    (<= depth 4) 15.0
    (>= depth 10) 0.0
    :else (* 15.0 (/ (- 10 depth) 6.0))))

(defn score-naming-consistency
  "Consistency ratio 0-1, scaled to 15 points."
  [ratio]
  (* 15.0 ratio))

(defn score-comment-ratio
  "Ideal: 10-30%. Penalty outside that range."
  [ratio]
  (cond
    (and (>= ratio 0.10) (<= ratio 0.30)) 15.0
    (< ratio 0.05) (* 15.0 (/ ratio 0.05))
    (< ratio 0.10) (+ (* 15.0 0.5) (* 15.0 0.5 (/ (- ratio 0.05) 0.05)))
    (> ratio 0.50) 0.0
    :else (* 15.0 (/ (- 0.50 ratio) 0.20))))

(defn score-complexity
  "Ideal: <= 0.1 branches per line. Penalty ramps from 0.1 to 0.4."
  [complexity]
  (cond
    (<= complexity 0.1) 15.0
    (>= complexity 0.4) 0.0
    :else (* 15.0 (/ (- 0.4 complexity) 0.3))))

(defn analyze-file [filepath]
  (let [content (slurp (str filepath))
        lines (str/split-lines content)
        lang (detect-language filepath)
        ll (line-length-metrics lines)
        avg-fn-len (avg-function-length lines lang)
        nest-depth (max-nesting-depth lines)
        name-consistency (naming-consistency lines)
        cmt-ratio (comment-ratio lines lang)
        complexity (cyclomatic-complexity-proxy lines lang)
        metrics {:avg-line-length (Math/round (:avg-line-length ll))
                 :max-line-length (:max-line-length ll)
                 :avg-function-length (Math/round (double avg-fn-len))
                 :max-nesting-depth nest-depth
                 :naming-consistency (Math/round (* 100.0 name-consistency))
                 :comment-ratio (Math/round (* 100.0 cmt-ratio))
                 :cyclomatic-complexity-proxy (Math/round (* 100.0 complexity))}
        scores {:avg-line-length (score-avg-line-length (:avg-line-length ll))
                :max-line-length (score-max-line-length (:max-line-length ll))
                :avg-function-length (score-avg-function-length avg-fn-len)
                :max-nesting-depth (score-nesting-depth nest-depth)
                :naming-consistency (score-naming-consistency name-consistency)
                :comment-ratio (score-comment-ratio cmt-ratio)
                :cyclomatic-complexity-proxy (score-complexity complexity)}
        total (Math/round (double (reduce + (vals scores))))]
    {:file (str filepath)
     :language (name lang)
     :lines (count lines)
     :score total
     :metrics metrics
     :scores scores}))

;; --- File discovery ---

(def scannable-extensions
  #{"clj" "cljs" "cljc" "py" "js" "jsx" "ts" "tsx" "java"
    "c" "h" "cpp" "hpp" "cc" "rb" "go" "rs" "sh" "bash"})

(defn find-source-files [dir]
  (->> (fs/glob dir "**")
       (filter fs/regular-file?)
       (filter #(let [ext (some-> (fs/extension %) str/lower-case)]
                  (scannable-extensions ext)))
       (remove #(str/includes? (str %) "node_modules"))
       (remove #(str/includes? (str %) ".git/"))
       (remove #(str/includes? (str %) "target/"))
       (remove #(str/includes? (str %) "vendor/"))
       (sort-by str)))

;; --- Output formatting ---

(defn format-text-result [result]
  (let [{:keys [file language lines score metrics]} result]
    (str/join "\n"
              [(format "  File: %s" file)
               (format "  Language: %s | Lines: %d | Score: %d/100" language lines score)
               (format "  Metrics:")
               (format "    Avg line length:       %d chars" (:avg-line-length metrics))
               (format "    Max line length:       %d chars" (:max-line-length metrics))
               (format "    Avg function length:   %d lines" (:avg-function-length metrics))
               (format "    Max nesting depth:     %d" (:max-nesting-depth metrics))
               (format "    Naming consistency:    %d%%" (:naming-consistency metrics))
               (format "    Comment ratio:         %d%%" (:comment-ratio metrics))
               (format "    Complexity proxy:      %d%%" (:cyclomatic-complexity-proxy metrics))
               ""])))

(defn format-text [results]
  (let [avg-score (if (empty? results) 0
                      (Math/round (double (/ (reduce + (map :score results))
                                             (count results)))))]
    (str/join "\n"
              (concat
               [(format "Code Readability Analysis (%d files)\n" (count results))
                (str (apply str (repeat 60 "=")) "\n")]
               (map format-text-result results)
               [(str (apply str (repeat 60 "=")))
                (format "Overall average score: %d/100" avg-score)]))))

(defn format-json [results]
  (let [avg-score (if (empty? results) 0
                      (Math/round (double (/ (reduce + (map :score results))
                                             (count results)))))]
    (json/generate-string
     {:files results
      :summary {:file-count (count results)
                :average-score avg-score}}
     {:pretty true})))

(defn format-edn [results]
  (let [avg-score (if (empty? results) 0
                      (Math/round (double (/ (reduce + (map :score results))
                                             (count results)))))]
    (pr-str {:files results
             :summary {:file-count (count results)
                       :average-score avg-score}})))

;; --- CLI ---

(def cli-spec
  {:dir       {:desc "Directory to scan" :default "." :alias :d}
   :format    {:desc "Output format: text, json, edn" :default "text" :alias :f}
   :threshold {:desc "Minimum score (exit 1 if any file is below)" :coerce :long :alias :t}
   :help      {:desc "Show help" :alias :h :coerce :boolean}})

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})]
    (when (:help opts)
      (println "code-readability-analyzer — Score code readability and suggest improvements")
      (println)
      (println (cli/format-opts {:spec cli-spec}))
      (System/exit 0))
    (let [dir (:dir opts)
          fmt (:format opts)
          threshold (:threshold opts)
          files (find-source-files dir)]
      (if (empty? files)
        (do (println (format "No source files found in %s" dir))
            (System/exit 0))
        (let [results (mapv analyze-file files)
              output (case fmt
                       "json" (format-json results)
                       "edn"  (format-edn results)
                       (format-text results))]
          (println output)
          (when threshold
            (let [failing (filter #(< (:score %) threshold) results)]
              (when (seq failing)
                (println (format "\nFAIL: %d file(s) scored below threshold %d:"
                                 (count failing) threshold))
                (doseq [f failing]
                  (println (format "  %s: %d/100" (:file f) (:score f))))
                (System/exit 1)))))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
