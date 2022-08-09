(ns paradigms-of-ai.part-2.general-problem-solver.v1-test
  (:require [clojure.test :refer :all]
            [paradigms-of-ai.part-2.general-problem-solver.v1 :refer :all]
            [clojure.string :as str]))

(def school-ops
  [{:action :drive-son-to-school
    :preconditions [:son-at-home :car-works]
    :add-list #{:son-at-school}
    :delete-list #{:son-at-home}}
   {:action :shop-install-battery
    :preconditions [:car-needs-battery :shop-knows-problem :shop-has-money]
    :add-list #{:car-works}
    :delete-list #{}}
   {:action :tell-shop-problem
    :preconditions [:in-communication-with-shop]
    :add-list #{:shop-knows-problem}
    :delete-list #{}}
   {:action :telephone-shop
    :preconditions [:know-phone-number]
    :add-list #{:in-communication-with-shop}
    :delete-list #{}}
   {:action :look-up-number
    :preconditions [:have-phone-book]
    :add-list #{:know-phone-number}
    :delete-list #{}}
   {:action :give-shop-money
    :preconditions [:have-money]
    :add-list #{:shop-has-money}
    :delete-list #{:have-money}}])

(def line-end
  (System/getProperty "line.separator"))

(def success
  (str "Success" line-end))

(def failure
  (str "Failure" line-end))

(defn action-list
  [actions]
  (->> actions
       (map #(str "Executing " % line-end))
       (str/join "")))

(defn successful-action-list
  [& actions]
  (str (action-list actions)
       success))

(defn failed-action-list
  [& actions]
  (str (action-list actions)
       failure))

(deftest successful-solve
  (is (= (successful-action-list
           :look-up-number
           :telephone-shop
           :tell-shop-problem
           :give-shop-money
           :shop-install-battery
           :drive-son-to-school)
         (with-out-str
           (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                [:son-at-school]
                school-ops)))))

(deftest failed-solve
  (is (= failure
         (with-out-str
           (gps #{:son-at-home :car-needs-battery :have-money}
                [:son-at-school]
                school-ops)))))

(deftest simple-solve
  (is (= (successful-action-list :drive-son-to-school)
         (with-out-str
           (gps #{:son-at-home :car-works}
                [:son-at-school]
                school-ops)))))

(deftest clobbered-sibling-goal-bug
  (is (= (successful-action-list
           :look-up-number
           :telephone-shop
           :tell-shop-problem
           :give-shop-money
           :shop-install-battery
           :drive-son-to-school)
         (with-out-str
           (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                [:have-money :son-at-school]
                school-ops)))))

(deftest clobbered-sibling-goal-ordering
  (is (= (failed-action-list
           :look-up-number
           :telephone-shop
           :tell-shop-problem
           :give-shop-money
           :shop-install-battery
           :drive-son-to-school)
         (with-out-str
           (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                [:son-at-school :have-money]
                school-ops)))))

(deftest recursive-ops
  (is (thrown?
        StackOverflowError
        (gps #{:son-at-home :car-needs-battery :have-money}
             [:son-at-school]
             (cons {:action :ask-phone-number
                    :preconditions [:in-communication-with-shop]
                    :add-list #{:know-phone-number}
                    :delete-list #{}}
                   school-ops)))))
