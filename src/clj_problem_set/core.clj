(ns clj-problem-set.core)
(require 'clojure.set)

(defn simple-recursion [x]
  (when (> x 0)
    (conj (simple-recursion (dec x)) x)))

(defn recur-recursion-fn [x]
  ; recur is bound to fn
  (println (str "loop-fn: " x))
  (when (> x 0)
    (recur (dec x))))

(defn recur-recursion-loop [x]
  ; recur is bound to fn
  (loop [i x]
   (println (str "loop-loop: " i))
   (when (> i 0)
     (recur (dec i)))))

(defn nil-key [k m]
  ; (def m {})
  ; (:a m) returns nil
  ; This function truely test to see if a map actually contains a nil value,
  ; given a key.
  (and (contains? m k) (nil? (m k))))

(defn my-last [[first-el & rest-el]]
  (if (empty? rest-el)
    first-el
    (recur rest-el)))

(defn penultimate [[first-el second-el & rest-el]]
  (if (= (count rest-el) 1)
    second-el
    (recur (into (vector second-el) rest-el))))

(defn my-nth [coll idx]
  (if (<= (count coll) idx)
    nil
    (loop [i idx
           head (first coll)
           tail (rest coll)]
      (if (> i 0)
          (recur (dec i) (first tail) (rest tail))
          head))))

(defn my-count [coll]
  (reduce + (map #(if-not (nil? %) 1) coll)))

(defn my-sum [col-seq]
  (reduce + col-seq))

(defn odd-only [col]
  (for [x col
        :when (= (mod x 2) 1)
        ]
    x))

(defn filter-odd [col]
  (filter #(odd? %) col))

(defn my-reverse [col-seq]
  (let [col-type (type col-seq)
        empty-col (cond
                    (= col-type clojure.lang.PersistentList) '()
                    (= col-type clojure.lang.PersistentVector) [])]
    (loop [cs col-seq
           ec empty-col]
      (if (not (empty? cs))
        (if (= col-type clojure.lang.PersistentList)
          (recur (rest cs) (cons (first cs) ec))
          (recur (rest cs) (into (vector (first cs)) ec)))
        ec))))

(defn palindrome [col-seq]
  (let [elem-cnt (count col-seq)]
    (cond (or (= elem-cnt 1) (= elem-cnt 0)) true
          (= (first col-seq) (last col-seq)) (recur (drop-last (rest col-seq)))
          :else false)))


(defn set-interval [callback-fn ms]
  (future (while true (do (Thread/sleep ms) (callback-fn)))))

(defn -main
  "I don't do a whole lot."
  []
  (println (my-reverse [1 2 3])))

