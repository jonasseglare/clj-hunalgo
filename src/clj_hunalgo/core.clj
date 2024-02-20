(ns clj-hunalgo.core)

;; Port of
;; https://github.com/aalmi/HungarianAlgorithm/blob/master/HungarianAlgorithm.java

(defn to-int-matrix [data]
  (into-array (map int-array data)))

(defn wrap-matrix-access [matrix]
  (fn
    ([i j] (-> matrix (aget i) (aget j)))
    ([i j x] (-> matrix (aget i) (aset j (double x))))))

(defn hungarian-algorithm-state [matrix]
  (let [mat (into-array Object (map (fn [row] (into-array Double/TYPE (map double row))) matrix))
        rows (count matrix)
        cols (count (first matrix))]
    {:matrix mat
     :matrixf (wrap-matrix-access mat)
     :rows rows
     :cols cols
     :square-in-row (long-array rows -1)
     :square-in-col (long-array cols -1)
     :row-is-covered (boolean-array rows false)
     :col-is-covered (boolean-array cols false)
     :starred-zeros-in-row (long-array rows -1)}))

(defn subtract-min-value-from-row [row]
  (let [min-value (apply min row)]
    (dotimes [i (count row)]
      (aset row i (- (aget row i) min-value)))))

(defn subtract-min-value-from-col [matrix col]
  (let [min-value (apply min (map #(aget % col) matrix))]
    (doseq [row matrix]
      (aset row col (- (aget row col) min-value)))))

(defn hungarian-algorithm-step1 [{:keys [rows cols matrix]}]
  (dotimes [i rows]
    (subtract-min-value-from-row (aget matrix i)))
  (dotimes [j cols]
    (subtract-min-value-from-col matrix j)))

(defn hungarian-algorithm-step2 [{:keys [rows cols matrixf square-in-row square-in-col]}]
  (let [row-has-square (boolean-array rows false)
        col-has-square (boolean-array cols false)
        visit-element (fn [i j]
                        (if (and (zero? (matrixf i j))
                                 (not (aget row-has-square i))
                                 (not (aget col-has-square j)))
                          (do (aset row-has-square i true)
                              (aset col-has-square j true)
                              (aset square-in-row i j)
                              (aset square-in-col j i)
                              false)
                          true))]
    (dotimes [i rows]
      (doseq [j (range cols)
              :while (visit-element i j)]
        nil))))

(defn hungarian-algorithm-step3 [{:keys [cols square-in-col col-is-covered]}]
  (dotimes [j cols]
    (aset col-is-covered j (not= -1 (aget square-in-col j)))))

(defn hungarian-algorithm-step4 [{:keys [rows cols row-is-covered col-is-covered matrix starred-zeros-in-row]}]
  (when-let [[i j] (first (for [i (range rows)
                                :when (not (aget row-is-covered i))
                                :let [row (aget matrix i)]
                                j (range cols)
                                :when (and (zero? (aget row j))
                                           (not (aget col-is-covered j)))]
                            [i j]))]
    (aset starred-zeros-in-row i j)
    [i j]))


(defn hungarian-algorithm-step6 [{:keys [square-in-col square-in-row
                                         starred-zeros-in-row
                                         row-is-covered
                                         col-is-covered
                                         rows cols]} [i j]]
  (let [K (loop [i i
                 j j
                 K [[i j]]]
            (let [i (aget square-in-col j)]
              (if (= -1 i)
                K
                (let [K (conj K [i j])
                      j (aget starred-zeros-in-row i)]
                  (if (= -1 j)
                    K
                    (recur i j (conj K [i j])))))))]
    (doseq [[i j] (distinct K)]
      (when (= i (aget square-in-col j))
        (aset square-in-col j -1)
        (aset square-in-row i -1))
      (when (= j (aget starred-zeros-in-row i))
        (aset square-in-row i j)
        (aset square-in-col j i)))
    (dotimes [i rows]
      (aset starred-zeros-in-row i -1)
      (aset row-is-covered i false))
    (dotimes [j cols]
      (aset col-is-covered j false))))

(defn hungarian-algorithm-step7 [{:keys [rows cols row-is-covered col-is-covered matrixf]}]
  (let [min-uncovered-value (apply min (for [i (range rows)
                                             :when (not (aget row-is-covered i))
                                             j (range cols)
                                             :when (not (aget col-is-covered j))]
                                         (matrixf i j)))]
    (when (< 0 min-uncovered-value)
      (dotimes [i rows]
        (dotimes [j cols]
          (let [step (fn [x] (matrixf i j (+ x (matrixf i j))))]
            (cond
              (and (aget row-is-covered i) (aget col-is-covered j)) (step min-uncovered-value)
              (and (not (aget row-is-covered i))
                   (not (aget col-is-covered j))) (step (- min-uncovered-value))
              :else nil)))))))

(defn all-columns-are-covered [{:keys [col-is-covered]}]
  (every? identity col-is-covered))

(defn hungarian-main-step [{:keys [square-in-row row-is-covered col-is-covered] :as state}]
  (let [main-zero (loop [main-zero (hungarian-algorithm-step4 state)]
                    (if main-zero
                      main-zero
                      (do (hungarian-algorithm-step7 state)
                          (recur (hungarian-algorithm-step4 state)))))
        [i _j] main-zero
        k (aget square-in-row i)]
    (if (= -1 k)
      (do (hungarian-algorithm-step6 state main-zero)
          (hungarian-algorithm-step3 state))
      (do (aset row-is-covered i true)
          (aset col-is-covered k false)
          (hungarian-algorithm-step7 state)))))

(defn find-optimal-assignment [{:keys [square-in-col] :as state}]
  (hungarian-algorithm-step1 state)
  (hungarian-algorithm-step2 state)
  (hungarian-algorithm-step3 state)
  (loop []
    (when-not (all-columns-are-covered state)
      (hungarian-main-step state)
      (recur)))
  (map-indexed vector square-in-col))

(defn select-trace [ksel isel trace]
  (first (filter (fn [[k i]]
                   (and (= k ksel) (= i isel)))
                 trace)))

(defn solve-assignment-problem-ji-pairs [data]
  (-> data
      hungarian-algorithm-state
      find-optimal-assignment))

(defn solve-assignment-problem-ij-pairs [data]
  (->> data
       solve-assignment-problem-ji-pairs
       (map (fn [[j i]] [i j]))
       (sort-by first)))
