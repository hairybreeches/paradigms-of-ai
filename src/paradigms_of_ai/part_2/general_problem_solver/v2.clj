(ns paradigms-of-ai.part-2.general-problem-solver.v2
  (:require [clojure.set :as set]))

(defn sequence-contains?
  [s x]
  (some #(= % x) s))

(defn appropriate-p
  "An op is appropriate to its goal if it is in its add-list"
  [goal op]
  (sequence-contains? (:add-list op) goal))

(declare achieve-all)

(defn apply-op
  "Return a new, transformed state if op is applicable"
  [state goal {:keys [preconditions action delete-list add-list]} goal-stack ops]
  (when-let [new-state (achieve-all state preconditions (cons goal goal-stack) ops)]
    (as-> new-state current-state
          (concat current-state [[:executing action]])
          (concat current-state add-list)
          (remove delete-list current-state))))

(defn achieve
  "A goal is achieved if it already holds or there is an appropriate op for it that is applicable"
  [state goal goal-stack ops]
  (cond
    (sequence-contains? state goal) state
    (sequence-contains? goal-stack goal) nil
    :else
    (->> (filter #(appropriate-p goal %) ops)
         (some #(apply-op state goal % goal-stack ops)))))

(defn achieve-each
  "Achieve each goal, and make sure they all remain true in the end"
  [state goals goal-stack ops]
  (let [possible-end-state (reduce
                             (fn [previous-state new-goal]
                               (achieve previous-state new-goal goal-stack ops))
                             state
                             goals)]
    (when (set/subset? (set goals) (set possible-end-state))
      possible-end-state)))

(defn orderings
  [l]
  [l])

(defn achieve-all
  "Achieve each goal, trying several orderings"
  [state goals goal-stack ops]
  (some #(achieve-each state % goal-stack ops) (orderings goals)))

(defn action?
  [state-message]
  (and (seqable? state-message)
       (= :executing (first state-message))))

(defn gps
  "General Problem Solver: achieve all goals using ops"
  [state goals ops]
  (some->> (achieve-all (conj state :start) goals nil ops)
           (filter action?)))
