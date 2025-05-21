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
           '{:query {:where [(missing? $ ?e :user/id)]}})))
  (testing "explicit source"
    (is (= (qb/where-missing {} '[$hist ?e :user/id])
           '{:query {:where [(missing? $hist ?e :user/id)]}}))))

(deftest test-data->query-vector
  (testing ":find"
    (let [q (qb/data->query ^{:find '?customer} [:customer/id 1])]
      (is (= q '{:query {:find [[?customer ...]], :where [[?customer :customer/id ?customer-id]], :in [?customer-id]}, :args [1]}))))

  (testing ":from"
    (let [q (qb/data->query ^{:from '?customer} [:customer/id 1])]
      (is (= q '{:query {:find [[?customer ...]], :where [[?customer :customer/id ?customer-id]], :in [?customer-id]}, :args [1]}))))

  (testing "with :find and :from"
    (let [q (qb/data->query ^{:find '?customer :from '?order} [:order/customer '?customer])]
      (is (= (:query q)
             '{:find [[?customer ...]]
               :where [[?order :order/customer ?customer]]})
          "query structure matches")))

  (testing "with :as and :find"
    (let [q (qb/data->query ^{:find '?user :as :user} [:user/id 1])]
      (is (= (:query q)
             '{:find [?user]
               :keys [:user]
               :where [[?user :user/id ?user-id]]
               :in [?user-id]})
          "query includes :keys from :as")
      (is (= (:args q) [1]))))

  (testing "with nil value"
    (is (= (qb/data->query [:user/id nil])
           '{:query {:find [[?e ...]], :where [(missing? $ ?e :user/id)]}})))

  (testing "with nil value and explicit source"
    (is (= (qb/data->query ^{:in '$hist} [:user/id nil])
           '{:query {:find [[?e ...]], :where [(missing? $hist ?e :user/id)]}})))

  (testing "aggregate sum"
    (let [q (qb/data->query ^{:aggregate ['sum :order/total]} [:order/id '_])]
      (is (= (:query q)
             '{:find [(sum ?order-total) .]
               :with [?e]
               :where [[?e :order/id]
                       [?e :order/total ?order-total]]}))))

  (testing "aggregate attribute"
    (let [q (qb/data->query ^{:aggregate :order/customer} [:order/id '_])]
      (is (= (:query q)
             '{:find [[?order-customer ...]]
               :with [?e]
               :where [[?e :order/id]
                       [?e :order/customer ?order-customer]]})))))

(deftest test-data->query-map
  (testing ":find"
    (let [q (qb/data->query ^{:find '?customer} {:customer/id 1})]
      (is (= q '{:query {:find [[?customer ...]], :where [[?customer :customer/id ?customer-id]], :in [?customer-id]}, :args [1]}))))

  (testing ":from"
    (let [q (qb/data->query ^{:from '?customer} {:customer/id 1})]
      (is (= q '{:query {:find [[?customer ...]], :where [[?customer :customer/id ?customer-id]], :in [?customer-id]}, :args [1]}))))

  (testing ":find and :from"
    (let [q (qb/data->query ^{:find '?customer :from '?order} {:order/customer '?customer})]
      (is (= (:query q)
             '{:find [[?customer ...]]
               :where [[?order :order/customer ?customer]]})
          "query structure matches")))

  (testing ":as and :find"
    (let [q (qb/data->query ^{:find '?user :as :user} {:user/id 1})]
      (is (= (:query q)
             '{:find [?user]
               :keys [:user]
               :where [[?user :user/id ?user-id]]
               :in [?user-id]})
          "query includes :keys from :as")
      (is (= (:args q) [1]))))

  (testing "with nil value"
    (is (= (qb/data->query {:user/id nil})
           '{:query {:find [[?e ...]], :where [(missing? $ ?e :user/id)]}})))

  (testing "with nil value and explicit source"
    (is (= (qb/data->query ^{:in '$hist} {:user/id nil})
           '{:query {:find [[?e ...]], :where [(missing? $hist ?e :user/id)]}})))

  (testing "aggregate sum"
    (let [q (qb/data->query ^{:aggregate ['sum :order/total]} {:order/id '_})]
      (is (= (:query q)
             '{:find [(sum ?order-total) .]
               :with [?e]
               :where [[?e :order/id]
                       [?e :order/total ?order-total]]}))))

  (testing "with aggregate attr"
    (let [q (qb/data->query ^{:aggregate :order/customer} {:order/id '_})]
      (is (= (:query q)
             '{:find [[?order-customer ...]]
               :with [?e]
               :where [[?e :order/id]
                       [?e :order/customer ?order-customer]]})))))

(deftest test-data->query-invalid
  (testing "Map with more than 8 keys"
    (is (thrown-with-msg?
         IllegalArgumentException
         #"Query order is not preserved for conditions map with more than 8 keys"
         (qb/data->query {:k1 1 :k2 2 :k3 3 :k4 4 :k5 5 :k6 6 :k7 7 :k8 8 :k9 9}))))
  (testing "Invalid conditions"
    (is (thrown-with-msg?
         IllegalArgumentException
         #"Cannot turn .* into query"
         (qb/data->query 12345)))))
