(ns ^:no-doc antq.report.table
  (:require
   [antq.log :as log]
   [antq.report :as report]
   [antq.util.dep :as u.dep]
   [antq.util.file :as u.file]
   [antq.util.report :as u.report]
   [antq.util.ver :as u.ver]
   [clojure.set :as set]
   [clojure.string :as str]))

(def ^:private progress (atom nil))

(defn- expand-children
  [parent-grouped-deps parent-deps]
  (->> parent-deps
       (sort u.dep/compare-deps)
       (mapcat (fn [parent-dep]
                 (->> (or (get parent-grouped-deps (:name parent-dep))
                          [])
                      (map #(assoc % :level (inc (:level parent-dep))))
                      (expand-children parent-grouped-deps)
                      (concat [parent-dep]))))))

(def ^:private column-labels
  {:current-age "age"
   :latest-age "age"
   :cooldown-version "cooldown"
   :cooldown-days "dur"
   :cooldown-age "age"})

(defn- column-label
  [column]
  (get column-labels column (name column)))

(defn- calc-max-length
  [column-name deps]
  (apply max (count (column-label column-name)) (map (comp count column-name) deps)))

(defn- generate-row
  [dep columns max-lengths]
  (->> columns
       (map-indexed (fn [i column]
                      (format (str "%-" (nth max-lengths i) "s")
                              (or (get dep column)
                                  ""))))
       (str/join " | ")
       (format "| %s |")))

(defn- apply-level
  [level s]
  (let [indent (apply str (repeat (* level 2) " "))]
    (str (when (seq indent)
           (str indent))
         s)))

(defn- print-table
  [options deps]
  (let [columns (cond-> (if (some (some-fn :current-age :latest-age) deps)
                          [:file :name :current :current-age :latest :latest-age]
                          [:file :name :current :latest])
                  (some :cooldown-days deps)
                  (conj :cooldown-version :cooldown-days :cooldown-age)

                  (:changes-in-table options)
                  (conj :changes-url))
        max-lengths (map #(calc-max-length % deps) columns)]
    (println (generate-row (->> columns
                                (map #(vector % (column-label %)))
                                (into {}))
                           columns max-lengths))
    (println (->> max-lengths
                  (map #(apply str (repeat % "-")))
                  (str/join "-|-")
                  (format "|-%s-|")))
    (doseq [dep deps]
      (println (generate-row dep columns max-lengths)))))

(defmethod report/reporter "table"
  [deps options]
  ;; Show table
  (if (empty? deps)
    (println "All dependencies are up-to-date.")
    (let [parent-grouped-deps (group-by :parent deps)]
      (->> (or (get parent-grouped-deps nil)
               [])
           (map #(assoc % :level 0))
           (expand-children parent-grouped-deps)
           (u.report/skip-duplicated-file-name)
           (map #(assoc % :latest-version (u.ver/normalize-latest-version %)))
           (map #(update % :file u.file/normalize-path))
           (map #(assoc % :current-age (some-> (:released-at %) u.report/age)
                        :latest-age (some-> (:latest-released-at %) u.report/age)
                        :cooldown-age (some-> (:cooldown-released-at %) u.report/age)
                        :cooldown-days (some-> (:cooldown-days %) (str "d"))))
           (map #(let [latest-key (if (seq (:latest-name %))
                                    :latest-name
                                    :latest-version)]
                   (set/rename-keys % {:version :current
                                       latest-key :latest})))
           (map #(update % :name (partial apply-level (or (:level %) 0))))
           (print-table options))))

  ;; Show changes URLs
  (when-not (:changes-in-table options)
    (let [urls (->> deps
                    (filter :latest-version)
                    (sort u.dep/compare-deps)
                    (keep :changes-url)
                    (distinct))]
      (when (seq urls)
        (println "\nAvailable changes:")
        (doseq [u urls]
          (println "-" u))))))

(defn- progress-text
  [{:keys [width total-count current-count]}]
  (let [width (or width 50)
        ratio (int (* width (/ current-count total-count)))]
    (format "[%s%s] %d/%d\r"
            (apply str (repeat ratio "#"))
            (apply str (repeat (- width ratio) "-"))
            current-count
            total-count)))

(defmethod report/init-progress "table"
  [deps _options]
  (reset! progress {:total-count (count deps)
                    :count-atom (atom 0)}))

(defmethod report/run-progress "table"
  [_dep _options]
  (when-let [{:keys [total-count count-atom]} @progress]
    (when (< @count-atom total-count)
      (swap! count-atom inc)
      (log/async-print
       (progress-text {:total-count total-count :current-count @count-atom})))))

(defmethod report/deinit-progress "table"
  [_ _]
  (println ""))
