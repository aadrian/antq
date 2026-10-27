(ns antq.report.table-test
  (:require
   [antq.report :as report]
   [antq.report.table]
   [antq.test-helper :as h]
   [antq.test-report]
   [clojure.string :as str]
   [clojure.test :as t]))

(defn- reporter
  [deps]
  (report/reporter deps {:reporter "table"}))

(t/deftest reporter-test
  (let [dummy-deps [(h/test-dep {:file "a" :name "foo" :version "1" :latest-version "2"})
                    (h/test-dep {:file "b" :name "bar" :version "1" :latest-version nil})
                    (h/test-dep {:file "c" :name "baz" :version "2" :latest-version "3" :changes-url "here"})
                    (h/test-dep {:file "c" :name "old" :version "1" :latest-version nil :latest-name "new"})]
        outputs (str/split-lines (with-out-str (reporter dummy-deps)))]
    (t/is (some #(and (str/includes? % "foo") (str/includes? % "1") (str/includes? % "2")) outputs))
    (t/is (some #(and (str/includes? % "bar") (str/includes? % "Failed to fetch")) outputs))
    (t/is (some #(and (str/includes? % "baz") (str/includes? % "2") (str/includes? % "3")) outputs))
    (t/is (some #(and (str/includes? % "old") (str/includes? % "new")) outputs))
    (t/is (some #(str/includes? % "here") outputs))

    (t/testing "no release dates, no age or cooldown columns"
      (t/is (not (str/includes? (first outputs) "age"))))

    ;; no outdated deps
    (t/is (str/includes? (with-out-str (reporter []))
                         "All dependencies are up-to-date"))))

(defn- days-ago
  [n]
  (str (.minus (java.time.Instant/now) (java.time.Duration/ofDays n))))

(defn- cells
  [row]
  (mapv str/trim (rest (str/split row #"\|"))))

(t/deftest reporter-cooldown-test
  (let [deps [(h/test-dep {:file "a" :name "foo" :version "1" :latest-version "3"
                           :released-at (days-ago 40) :latest-released-at (days-ago 2)
                           :cooldown-days 7 :cooldown-version "2"})
              (h/test-dep {:file "a" :name "bar" :version "1" :latest-version "2"
                           :released-at (days-ago 9) :latest-released-at nil
                           :cooldown-days 7 :cooldown-version nil})
              (h/test-dep {:file "a" :name "baz" :version "1" :latest-version "2"})]
        [header _ & rows] (str/split-lines (with-out-str (reporter deps)))]
    (t/is (= [":file" ":name" ":current" ":latest" ":current-age" ":latest-age" ":cooldown-version" ":cooldown-days"]
             (cells header)))
    (t/is (= [["a" "bar" "1" "2" "9d" "" "" "7"]
              ["" "baz" "1" "2" "" "" "" ""]
              ["" "foo" "1" "3" "40d" "2d" "2" "7"]]
             (map cells (take 3 rows))))))
