(ns antq.util.report-test
  (:require
   [antq.test-report]
   [antq.util.report :as sut]
   [clojure.test :as t]))

(defn- ago
  [hours]
  (str (.minus (java.time.Instant/now) (java.time.Duration/ofHours hours))))

(t/deftest age-test
  (t/are [expected hours] (= expected (sut/age (ago hours)))
    "0h" 0
    "23h" 23
    "1d" 24
    "1d" 47
    "120d" (* 120 24)
    "0h" -5))

(t/deftest skip-duplicated-file-name-test
  (t/is (= [{:file "foo"}
            {:file "bar"}
            {:file "baz"}]
           (sut/skip-duplicated-file-name
            [{:file "foo"}
             {:file "bar"}
             {:file "baz"}])))

  (t/is (= [{:file "foo"}
            {:file ""}
            {:file "bar"}]
           (sut/skip-duplicated-file-name
            [{:file "foo"}
             {:file "foo"}
             {:file "bar"}])))

  (t/is (= [{:file "bar"}
            {:file "baz"}
            {:file ""}]
           (sut/skip-duplicated-file-name
            [{:file "bar"}
             {:file "baz"}
             {:file "baz"}])))

  (t/is (= [{:file "foo"}
            {:file ""}
            {:file ""}
            {:file "bar"}]
           (sut/skip-duplicated-file-name
            [{:file "foo"}
             {:file ""}
             {:file "foo"}
             {:file "bar"}])))

  (t/is (= [{:file "foo"}
            {:file ""}
            {:file "bar"}
            {:file ""}]
           (sut/skip-duplicated-file-name
            [{:file "foo"}
             {:file ""}
             {:file "bar"}
             {:file "bar"}]))))
