(ns paradigms-of-ai.part-2.general-problem-solver.v2-test
  (:require [clojure.test :refer :all]
            [paradigms-of-ai.part-2.general-problem-solver.v2 :refer :all]))

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

(defn successful-action-list
  [& actions]
  (map (fn [action] [:executing action]) actions))

(deftest successful-solve
  (is (= (successful-action-list
           :look-up-number
           :telephone-shop
           :tell-shop-problem
           :give-shop-money
           :shop-install-battery
           :drive-son-to-school)
         (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
              [:son-at-school]
              school-ops))))

(deftest failed-solve
  (is (= nil
         (gps #{:son-at-home :car-needs-battery :have-money}
              [:son-at-school]
              school-ops))))

(deftest simple-solve
  (is (= (successful-action-list :drive-son-to-school)
         (gps [:son-at-home :car-works]
              [:son-at-school]
              school-ops))))

(deftest already-succeeding
  (is (= (successful-action-list)
         (gps [:son-at-home]
              [:son-at-home]
              school-ops))))

(deftest clobbered-sibling-goal-bug
  (is (= nil
         (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
              [:have-money :son-at-school]
              school-ops))))

(deftest clobbered-sibling-goal-ordering
  (is (= nil
         (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
              [:son-at-school :have-money]
              school-ops))))

(deftest recursive-ops
  (is (= nil
         (gps #{:son-at-home :car-needs-battery :have-money}
              [:son-at-school]
              (cons {:action :ask-phone-number
                     :preconditions [:in-communication-with-shop]
                     :add-list #{:know-phone-number}
                     :delete-list #{}}
                    school-ops)))))

(def banana-ops
  [{:action :climb-on-chair
    :preconditions [:chair-at-middle-room :at-middle-room :on-floor]
    :add-list #{:at-bananas :on-chair}
    :delete-list #{:at-middle-room :on-floor}}
   {:action :push-chair-from-door-to-middle-room
    :preconditions [:chair-at-door :at-door]
    :add-list #{:chair-at-middle-room :at-middle-room}
    :delete-list #{:chair-at-door :at-door}}
   {:action :walk-from-door-to-middle-room
    :preconditions [:at-door :on-floor]
    :add-list #{:at-middle-room}
    :delete-list #{:at-door}}
   {:action :grasp-bananas
    :preconditions [:at-bananas :empty-handed]
    :add-list #{:has-bananas}
    :delete-list #{:empty-handed}}
   {:action :drop-ball
    :preconditions [:has-ball]
    :add-list #{:empty-handed}
    :delete-list #{:has-ball}}
   {:action :eat-bananas
    :preconditions [:has-bananas]
    :add-list #{:empty-handed :not-hungry}
    :delete-list #{:has-bananas :hungry}}])

(deftest banana-case
  (is (= (successful-action-list :push-chair-from-door-to-middle-room
                                 :climb-on-chair
                                 :drop-ball
                                 :grasp-bananas
                                 :eat-bananas)
         (gps [:at-door :on-floor :has-ball :hungry :chair-at-door]
              [:not-hungry]
              banana-ops))))
