(ns paradigms-of-ai.part-2.general-problem-solver.v1
  (:require [clojure.set :as set]))

(def state (atom nil))
(def ops (atom nil))

(declare achieve)

(defn appropriate-p
  "An op is appropriate to its goal if it is in its add-list"
  [goal op]
  (contains? (:add-list op) goal))

(defn apply-op
  "Print a message and update state if op is applicable"
  [{:keys [preconditions action delete-list add-list]}]
  (when (every? achieve preconditions)
    (println "Executing" action)
    (swap! state
           #(-> %
                (set/difference delete-list)
                (set/union add-list)))))

(defn achieve
  "A goal is achieved if it already holds, or if there is an appropriate op for it that is applicable"
  [goal]
  (or (contains? @state goal)
      (->> @ops
           (filter #(appropriate-p goal %))
           (some apply-op))))

(defn gps
  "General Problem Solver: achieve all goals using ops"
  [start-state goals available-ops]
  (reset! state start-state)
  (reset! ops available-ops)
  (if (every? achieve goals)
    true
    false))
