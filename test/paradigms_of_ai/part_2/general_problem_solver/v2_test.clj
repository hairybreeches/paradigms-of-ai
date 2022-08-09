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

(defn make-maze-op
  [here there]
  {:action [:move :from here :to there]
   :preconditions [[:at here]]
   :add-list #{[:at there]}
   :delete-list #{[:at here]}})

(defn make-maze-ops
  [[x y]]
  [(make-maze-op x y)
   (make-maze-op y x)])

(def maze-ops
  (mapcat make-maze-ops
          [[1 2] [2 3] [3 4] [4 9] [9 14] [9 8] [8 7] [7 12] [12 13] [12 11] [11 6] [11 16] [16 17] [17 22] [21 22]
           [22 23] [23 18] [23 24] [24 19] [19 20] [20 15] [15 10] [10 5] [20 25]]))

(defn maze-path
  [moves]
  (let [from-to-pairs (->> moves
                           (map second)
                           (map #(drop 2 %))
                           (map (fn [[from _ to]] [from to])))]
    (cons (first (first from-to-pairs))
          (map second from-to-pairs))))

(deftest maze-solving
  (is (= [1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25]
         (maze-path
           (gps [[:at 1]] [[:at 25]] maze-ops)))))

(defn move-ons
  [a b c]
  (if (= b :table)
    #{[a :on c]}
    #{[a :on c] [:space :on b]}))

(defn move-op
  [a b c]
  {:action [:move a :from b :to c]
   :preconditions [[:space :on a] [:space :on c] [a :on b]]
   :add-list (move-ons a b c)
   :delete-list (move-ons a c b)})

(defn make-block-ops
  [blocks]
  (mapcat
    identity
    (for [a blocks
          b blocks
          :when (not= a b)]
      (concat
        [(move-op a :table b)
         (move-op a b :table)]
        (for [c (remove #{a b} blocks)]
          (move-op a b c))))))

(deftest simple-blocks
  (is (= [[:executing [:move :a :from :table :to :b]]]
         (gps [[:a :on :table] [:b :on :table] [:space :on :a] [:space :on :b] [:space :on :table]]
              [[:a :on :b] [:b :on :table]]
              (make-block-ops [:a :b])))))

(deftest more-complex-blocks
  (is (= [[:executing [:move :a :from :b :to :table]]
          [:executing [:move :b :from :table :to :a]]]
         (gps [[:a :on :b] [:b :on :table] [:space :on :a] [:space :on :table]]
              [[:b :on :a] [:a :on :table]]
              (make-block-ops [:a :b])))))
