(ns code-readability-analyzer.specs
  "Data specs for code-readability-analyzer (https://clojure.org/guides/spec).
  Function specs (s/fdef) live next to each defn in code_readability_analyzer.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            ;; entity namespaces for map keys that repeat with other meanings:
            ;; :avg-line-length is a raw number, a rounded metric and a score
            [code-readability-analyzer.line-lengths :as-alias line-lengths]
            [code-readability-analyzer.metrics :as-alias metrics]
            [code-readability-analyzer.scores :as-alias scores]))

;; Generators are built inside fns, never in top-level defs:
;; clojure.spec.gen.alpha loads test.check on first use, and the JVM runtime
;; classpath (deps.edn :deps) has no test.check.

;; --- Inputs ---

(s/def ::lang #{:clojure :python :javascript :java :c :cpp :ruby :go :rust :shell :unknown})

(def ^:private code-fragments
  ["(defn score [x]" "  (if (pos? x) {:a [1 2]} nil))" "; a comment" ";; another_comment"
   "def load_file(path):" "    if path and other_path:" "    # TODO tidy" "    return None"
   "function loadFile(path) {" "  const userName = getUser(id);" "  // explain this" "}"
   "public static int maxValue(int[] xs) {" "fn parse_input(s: &str) -> Vec<u8> {"
   "func (s *Server) Run() error {" "int main(int argc, char **argv) {" "build() {"
   "    elif x < 0:" "} else if (a && b || c) {" "x = [[[1]]]" "" "   "
   (str/join (repeat 130 "x"))])

(defn- gen-line []
  (gen/one-of [(gen/elements code-fragments)
               (gen/fmap (fn [[n s]] (str (str/join (repeat n " ")) s))
                         (gen/tuple (gen/choose 0 12) (gen/elements code-fragments)))
               (gen/string-alphanumeric)]))

;; One line of source text, as produced by clojure.string/split-lines.
(s/def ::line-text
  (s/with-gen (s/and string? #(not (re-find #"[\r\n]" %))) gen-line))

(s/def ::source-lines (s/coll-of ::line-text :kind vector? :gen-max 40))

;; A relative or absolute file path, as a string or java.nio.file.Path.
(defn- gen-path-string []
  (gen/fmap (fn [[dirs base ext]]
              (str/join "/" (conj dirs (cond-> base ext (str "." ext)))))
            (gen/tuple (gen/vector (gen/not-empty (gen/string-alphanumeric)) 0 3)
                       (gen/not-empty (gen/string-alphanumeric))
                       (gen/one-of [(gen/return nil)
                                    (gen/elements ["py" "js" "tsx" "clj" "go" "rs" "sh" "md" "PY"])
                                    (gen/string-alphanumeric)]))))

(s/def ::path-like
  (s/with-gen (s/or :string (s/and string? seq)
                    :path #(instance? java.nio.file.Path %))
    gen-path-string))

;; An identifier as core/identifier-pattern captures it.
(s/def ::identifier
  (s/with-gen (s/and string? #(re-matches #"[a-z][a-zA-Z0-9_]*" %))
    #(gen/fmap (fn [[c cs]] (apply str c cs))
               (gen/tuple (gen/elements "abcxyz")
                          (gen/vector (gen/elements "abcXYZ019_") 0 12)))))

(s/def ::name-class #{:snake_case :camelCase :mixed :neutral})

;; --- Metric values ---

(s/def ::length
  (s/with-gen (s/and number? #(not (neg? %)))
    #(gen/one-of [(gen/choose 0 300)
                  (gen/double* {:min 0 :max 300 :NaN? false :infinite? false})])))
(s/def ::depth nat-int?)
(s/def ::ratio (s/double-in :min 0.0 :max 1.0 :NaN? false))
;; branch keywords per non-blank line; can exceed 1
(s/def ::density
  (s/with-gen (s/and double? #(not (neg? %)))
    #(gen/double* {:min 0 :max 3 :NaN? false :infinite? false})))

(s/def ::points-15 (s/double-in :min 0.0 :max 15.0 :NaN? false))
(s/def ::points-10 (s/double-in :min 0.0 :max 10.0 :NaN? false))

;; line-length-metrics
(s/def ::line-lengths/avg-line-length ::length)
(s/def ::line-lengths/max-line-length nat-int?)
(s/def ::line-lengths
  (s/keys :req-un [::line-lengths/avg-line-length ::line-lengths/max-line-length]))

;; --- Per-file result (analyze-file) ---

;; rounded metrics
(s/def ::metrics/avg-line-length nat-int?)
(s/def ::metrics/max-line-length nat-int?)
(s/def ::metrics/avg-function-length nat-int?)
(s/def ::metrics/max-nesting-depth nat-int?)
(s/def ::metrics/naming-consistency (s/int-in 0 101))
(s/def ::metrics/comment-ratio (s/int-in 0 101))
(s/def ::metrics/cyclomatic-complexity-proxy nat-int?)
(s/def ::metrics
  (s/keys :req-un [::metrics/avg-line-length ::metrics/max-line-length
                   ::metrics/avg-function-length ::metrics/max-nesting-depth
                   ::metrics/naming-consistency ::metrics/comment-ratio
                   ::metrics/cyclomatic-complexity-proxy]))

;; points per metric; they add up to at most 100
(s/def ::scores/avg-line-length ::points-15)
(s/def ::scores/max-line-length ::points-10)
(s/def ::scores/avg-function-length ::points-15)
(s/def ::scores/max-nesting-depth ::points-15)
(s/def ::scores/naming-consistency ::points-15)
(s/def ::scores/comment-ratio ::points-15)
(s/def ::scores/cyclomatic-complexity-proxy ::points-15)
(s/def ::scores
  (s/keys :req-un [::scores/avg-line-length ::scores/max-line-length
                   ::scores/avg-function-length ::scores/max-nesting-depth
                   ::scores/naming-consistency ::scores/comment-ratio
                   ::scores/cyclomatic-complexity-proxy]))

(s/def ::file string?)
(s/def ::language #{"clojure" "python" "javascript" "java" "c" "cpp" "ruby" "go" "rust"
                    "shell" "unknown"})
(s/def ::lines nat-int?)
(s/def ::score (s/int-in 0 101))
(s/def ::result (s/keys :req-un [::file ::language ::lines ::score ::metrics ::scores]))
(s/def ::results (s/coll-of ::result :kind sequential? :gen-max 5))

;; --- CLI option table (the babashka.cli :spec map) ---

(s/def ::desc string?)
(s/def ::default string?)
(s/def ::alias simple-keyword?)
(s/def ::coerce #{:boolean :string :int :long :double :keyword :symbol})
(s/def ::cli-option (s/keys :req-un [::desc] :opt-un [::default ::alias ::coerce]))
(s/def ::cli-spec (s/map-of simple-keyword? ::cli-option))
