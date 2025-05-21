(ns fmnoise.qb-test
  (:require [clojure.test :refer [deftest testing is]]
            [fmnoise.qb :as qb]))

(deftest test-add-input
  (testing "add single scalar input"
    (let [q {:query {} :args []}
          res (qb/add-input q '?x 42)]
      (is (= (:query res) {:in ['?x]}))
      (is (= (:args res) [42]))))

  (testing "add collection input"
    (let [q nil
          res (qb/add-input q '?xs #{1 2 3})]
      (is (= (:query res) {:in ['?xs]}))
      (is (= (:args res) [#{1 2 3}])))))

(deftest test-in
  (testing "simple use with default binding"
    (let [q nil
          res (qb/in q 42)]
      (is (= (:query res) {:in ['$]}))
      (is (= (:args res) [42]))))

  (testing "in with multiple bindings and values"
    (let [q nil
          res (qb/in q '?x 1 '?y 2)]
      (is (= (:query res) {:in ['?x '?y]}))
      (is (= (:args res) [1 2])))))

(deftest test-find
  (testing "find with vector bindings"
    (let [q {:query {}}
          res (qb/find q ['?x '?y])]
      (is (= (:query res) {:find ['?x '?y]}))))

  (testing "find with map bindings"
    (let [q {:query {}}
          res (qb/find q {:x '?x :y '?y})]
      (is (= (:query res) {:find ['?x '?y]
                           :keys [:x :y]})))))

(deftest test-find-coll
  (testing "find-coll wraps binding in ellipsis"
    (is (= (:query (qb/find-coll {} '?x))
           '{:find [[?x ...]]}))))

(deftest test-find-scalar
  (testing "find-scalar returns dotted binding"
    (is (= (:query (qb/find-scalar {} '(count ?x)))
           {:find ['(count ?x) '.]}))))

(deftest test-data->query-map
  (testing "Simple map with :find"
    (let [q (qb/data->query ^{:find '?user} {:user/id 1})]
      (is (= (:query q)
             '{:find [[?user ...]]
               :where [[?user :user/id ?user-id]]
               :in [?user-id]})
          "query structure matches")
      (is (= (:args q) [1]))))

  (testing "Map with :as and :find"
    (let [q (qb/data->query ^{:find '?user :as :user} {:user/id 1})]
      (is (= (:query q)
             '{:find [?user]
               :keys [:user]
               :where [[?user :user/id ?user-id]]
               :in [?user-id]})
          "query includes :keys from :as")
      (is (= (:args q) [1])))))

(deftest test-binding
  (testing "keyword binding"
    (is (= (qb/->binding :user/id) '?user-id)))

  (testing "string binding"
    (is (= (qb/->binding "x") '?x)))

  (testing "symbol binding with namespace"
    (is (= (qb/->binding 'user/id) '?user-id)))

  (testing "keyword with unique suffix"
    (let [b1 (qb/->binding :user/id true)
          b2 (qb/->binding :user/id true)]
      (is (not= b1 b2))
      (is (re-find #"^\?user-id_" (str b1))))))

(deftest test-with
  (testing "adds :with clause"
    (is (= (qb/with nil '?x '?y)
           {:query {:with ['?x '?y]}}))))

(deftest test-or-join
  (testing "simple or-join"
    (let [q nil
          res (qb/or-join q
                          '[[?e]
                            [?e :user/id ?id]
                            [?e :user/email ?email]]
                          '{?id 1 ?email "alex@google.com"})]
      (is (= res '{:query {:where [(or-join [?e] [?e :user/id ?id] [?e :user/email ?email])], :in [?id ?email]}, :args [1 "alex@google.com"]})))))

(deftest test-exclude
  (testing "exclude with default input-name"
    (is (= (qb/exclude {} '?user-id [1 2])
           '{:query {:where [(not [(?user-id-excluded ?user-id)])], :in [?user-id-excluded]}, :args [#{1 2}]})))

  (testing "exclude with custom input-name"
    (is (= (qb/exclude {} '?user-id [1 2] '?exluded-ids)
           '{:query {:where [(not [(?exluded-ids ?user-id)])], :in [?exluded-ids]}, :args [#{1 2}]}))))

(deftest test-where
  (testing "with 2 args"
    (is (= (qb/where nil '[?e :user/id 1])
           '{:query {:where [[?e :user/id 1]]}})))

  (testing "with 3 args"
    (is (= (qb/where nil '[?e :user/id] 1)
           '{:query {:where [[?e :user/id]], :in [:user/id]}, :args [1]}))))

(deftest test-where?
  (testing "when non-nil value is supplied"
    (is (= (qb/where? nil ['?e :user/id] 1)
           '{:query {:where [[?e :user/id]], :in [:user/id]}, :args [1]})))

  (testing "when nil is supplied"
    (is (= (qb/where? {} ['?e :user/id] nil)
           {}))))

(deftest test-where-star
  (is (= (qb/where* {} '[[?e :user/id 1] [?e :user/name "Alex"]])
         '{:query {:where [[?e :user/id 1] [?e :user/name "Alex"]]}})))

(deftest test-where-arrow
  (is (= (qb/where-> {} '[?e :user/id 1] '[?e :user/name "Alex"])
         '{:query {:where [[?e :user/id 1] [?e :user/name "Alex"]]}})))

(deftest test-where-not-star
  (is (= (qb/where-not* {} '[[?e :user/id 1] [?e :user/active false]])
         '{:query {:where [(not [?e :user/id 1]) (not [?e :user/active false])]}})))

(deftest test-where-not
  (testing "single value"
    (is (= (qb/where-not {} '[?e :user/id])
           '{:query {:where [(not [?e :user/id])]}})))
  (testing "set value"
    (is (= (qb/where-not {} '[?e :user/id ?id] #{1 2})
           '{:query {:where [[?e :user/id ?id] (not [(?id-excluded ?id)])], :in [?id-excluded]}, :args [#{1 2}]}))))

(deftest test-where-missing
  (testing "implicit source"
    (is (= (qb/where-missing {} '[?e :user/id])
           '{:query {:where [(missing $ ?e :user/id)]}})))
  (testing "explicit source"
    (is (= (qb/where-missing {} '[$hist ?e :user/id])
           '{:query {:where [(missing $hist ?e :user/id)]}}))))

(deftest test-data->query-vector
  (testing "Vector with aggregate sum"
    (let [q (qb/data->query ^{:aggregate ['sum :order/total]} [:order/id '_])]
      (is (= (:query q)
             '{:find [(sum ?order-total) .]
               :where [[?e :order/id]
                       [?e :order/total ?order-total]]}))))

  (testing "Vector with aggregate (non-func)"
    (let [q (qb/data->query ^{:aggregate :order/customer} [:order/id '_])]
      (is (= (:query q)
             '{:find [[?order-customer ...]]
               :where [[?e :order/id]
                       [?e :order/customer ?order-customer]]})))))

(deftest test-data->query-default
  (testing "Fallback to vector->query"
    (let [q (qb/data->query [:user/id 42])]
      (is (= (:query q)
             '{:find [[?e ...]]
               :where [[?e :user/id ?user-id]]
               :in [?user-id]})
          "uses default ?e binding")
      (is (= (:args q) [42]))))

  (testing "Invalid conditions"
    (is (thrown-with-msg?
         IllegalArgumentException
         #"Cannot turn .* into query"
         (qb/data->query 12345)))))
