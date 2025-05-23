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
  (is (= (qb/or-join nil '[[?e] [?e :user/id ?id] [?e :user/email ?email]] '{?id 1 ?email "alex@google.com"})
         '{:query {:where [(or-join [?e] [?e :user/id ?id] [?e :user/email ?email])], :in [?id ?email]}, :args [1 "alex@google.com"]})))

(deftest test-not-join
  (is (= (qb/not-join nil '[[?e] [?e :user/id ?id] [?e :user/email ?email]] '{?id 1 ?email "alex@google.com"})
         '{:query {:where [(not-join [?e] [?e :user/id ?id] [?e :user/email ?email])], :in [?id ?email]}, :args [1 "alex@google.com"]})))

(deftest test-exclude
  (testing "default input-name"
    (is (= (qb/exclude {} '?user-id [1 2])
           '{:query {:where [(not [(?user-id-input ?user-id)])], :in [?user-id-input]}, :args [#{1 2}]})))

  (testing "custom input-name"
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
           '{:query {:where [[?e :user/id ?id] (not [(?id-input ?id)])], :in [?id-input]}, :args [#{1 2}]}))))

(deftest test-where-missing
  (testing "implicit source"
    (is (= (qb/where-missing {} '[?e :user/id])
           '{:query {:where [[(missing? $ ?e :user/id)]]}})))
  (testing "explicit source"
    (is (= (qb/where-missing {} '[$hist ?e :user/id])
           '{:query {:where [[(missing? $hist ?e :user/id)]]}}))))

(deftest test-query
  (testing "no options"
    (testing "normal value"
      (is (= (qb/query {:customer/id 1})
             (qb/query [:customer/id 1])
             '{:query {:find [[?e ...]], :where [[?e :customer/id ?customer-id]], :in [?customer-id]}, :args [1]})))

    (testing "placeholder value _"
      (is (= (qb/query {:customer/id '_})
             (qb/query [:customer/id '_])
             '{:query {:find [[?e ...]], :where [[?e :customer/id]]}})))

    (testing "symbol value"
      (is (= (qb/query '{:customer/id ?id})
             (qb/query '[:customer/id ?id])
             '{:query {:find [[?e ...]], :where [[?e :customer/id ?id]]}})))

    (testing "negation !"
      (is (= (qb/query {:user/admin? ['! true]})
             (qb/query [:user/admin? ['! true]])
             (qb/query {:user/admin? '(! true)})
             (qb/query [:user/admin? '(! true)])
             '{:query {:find [[?e ...]], :where [(not [?e :user/admin? ?user-admin?])], :in [?user-admin?]}, :args [true]})))

    (testing "function with agrument"
      (is (= (qb/query {:customer/id '(> 1)})
             (qb/query {:customer/id ['> 1]})
             (qb/query [:customer/id '(> 1)])
             (qb/query [:customer/id ['> 1]])
             '{:query {:find [[?e ...]], :where [[?e :customer/id ?customer-id] [(> ?customer-id ?customer-id-input)]], :in [?customer-id-input]}, :args [1]})))

    (testing "function without agrument"
      (is (= (qb/query {:customer/id '(odd?)})
             (qb/query {:customer/id ['odd?]})
             (qb/query [:customer/id '(odd?)])
             (qb/query [:customer/id ['odd?]])
             '{:query {:find [[?e ...]], :where [[?e :customer/id ?customer-id] [(odd? ?customer-id)]]}})))

    (testing "raw condition"
      (is (= (qb/query '{any-symbol [(get-else $ ?e :payment/action :payment.action/none) ?action]})
             (qb/query '[any-symbol [(get-else $ ?e :payment/action :payment.action/none) ?action]])
             '{:query {:find [[?e ...]], :where [[(get-else $ ?e :payment/action :payment.action/none) ?action]]}}))))

  (testing "query options"
    (testing ":find"
      (is (= (qb/query {:find '?customer :where {:customer/id 1}})
             (qb/query {:find '?customer :where [:customer/id 1]})
             '{:query {:find [[?customer ...]], :where [[?customer :customer/id ?customer-id]], :in [?customer-id]}, :args [1]})))

    (testing ":from"
      (is (= (qb/query {:from '?customer :where {:customer/id 1}})
             (qb/query {:from '?customer :where [:customer/id 1]})
             '{:query {:find [[?customer ...]], :where [[?customer :customer/id ?customer-id]], :in [?customer-id]}, :args [1]})))

    (testing ":find and :from"
      (is (= (qb/query {:find '?customer :from '?order :where {:order/customer '?customer}})
             (qb/query {:find '?customer :from '?order :where [:order/customer '?customer]})
             '{:query
               {:find [[?customer ...]]
                :where [[?order :order/customer ?customer]]}})
          "query structure matches"))

    (testing ":nil"
      (testing "default"
        (is (= (qb/query {:user/name "Alex" :user/id nil})
               (qb/query [:user/name "Alex" :user/id nil])
               '{:query {:find [[?e ...]], :where [[?e :user/name ?user-name] [(missing? $ ?e :user/id)]], :in [?user-name]}, :args ["Alex"]})))

      (testing ":nil = :not"
        (is (= (qb/query {:nil :not :where {:user/name "Alex" :user/id nil}})
               (qb/query {:nil :not :where [:user/name "Alex" :user/id nil]})
               '{:query {:find [[?e ...]], :where [[?e :user/name ?user-name] (not [?e :user/id])], :in [?user-name]}, :args ["Alex"]})))

      (testing ":nil = :skip"
        (is (= (qb/query {:nil :skip :where {:user/name "Alex" :user/id nil}})
               (qb/query {:nil :skip :where [:user/name "Alex" :user/id nil]})
               '{:query {:find [[?e ...]], :where [[?e :user/name ?user-name]], :in [?user-name]}, :args ["Alex"]})))

      (testing ":nil = :missing"
        (is (= (qb/query {:nil :missing :where {:user/name "Alex" :user/id nil}})
               (qb/query {:nil :missing :where [:user/name "Alex" :user/id nil]})
               '{:query {:find [[?e ...]], :where [[?e :user/name ?user-name] [(missing? $ ?e :user/id)]], :in [?user-name]}, :args ["Alex"]})))

      (testing ":nil = :missing (default) and explicit source"
        (is (= (qb/query {:in '$hist :where {:user/id nil}})
               (qb/query {:in '$hist :where [:user/id nil]})
               (qb/query {:in '$hist :nil :missing :where {:user/id nil}})
               (qb/query {:in '$hist :nil :missing :where [:user/id nil]})
               '{:query {:find [[?e ...]], :where [[(missing? $hist ?e :user/id)]]}}))))

    (testing "aggregate"
      (testing "attr"
        (is (= (qb/query {:aggregate :order/customer :where {:order/id '_}})
               (qb/query {:aggregate :order/customer :where [:order/id '_]})
               '{:query {:find [[?order-customer ...]], :where [[?e :order/id] [?e :order/customer ?order-customer]], :with [?e]}})))

      (testing "func"
        (is (= (qb/query {:aggregate 'max :where {:order/id '_}})
               (qb/query {:aggregate 'max :where [:order/id '_]})
               '{:query {:find [(max ?e) .], :where [[?e :order/id]]}})))

      (testing "func and attr"
        (is (= (qb/query {:aggregate ['sum :order/total] :where {:order/id '_}})
               (qb/query {:aggregate ['sum :order/total] :where [:order/id '_]})
               '{:query {:find [(sum ?order-total) .], :where [[?e :order/id] [?e :order/total ?order-total]], :with [?e]}})))))

  (testing "illegal input"
    (testing "Map with more than 8 keys"
      (is (thrown-with-msg?
           IllegalArgumentException
           #"Query order is not preserved for conditions map with more than 8 keys"
           (qb/query {:k1 1 :k2 2 :k3 3 :k4 4 :k5 5 :k6 6 :k7 7 :k8 8 :k9 9}))))

    (testing "Vector with odd number of values"
      (is (thrown-with-msg?
           IllegalArgumentException
           #"Vector should have even number of values"
           (qb/query [:k1 1 :k2]))))))
