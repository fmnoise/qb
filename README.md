# qb

Minimalistic toolbox for building conditional Datalog queries

<p align="center"><img src="https://user-images.githubusercontent.com/4033391/214610859-8f9aedac-4ea9-404b-866f-42dd057100f3.png" width="200"></p>

## Usage

```clj
(defn orders-total-query [{:keys [customer-id order-id]}]
  (-> (find '[?c ?o (sum ?a)])
      (where? '[?o :order/id ?id] order-id)
      (where* '[[?o :order/customer ?c]
                [?o :order/items ?i]
                [?i :item/amount ?a]])
      (where? '[?c :customer/id ?cid] customer-id)
      (where-not '[?o :order/status :cancelled])))
      
(orders-total-query nil)
;; {:query {:find [?c ?o (sum ?a)]
;;          :where [[?o :order/customer ?c]
;;                  [?o :order/items ?i]
;;                  [?i :item/amount ?a]
;;                  (not [?o :order/status :cancelled])]}}

(orders-total-query {:customer-id 100})
;; {:query {:find [?c ?o (sum ?a)],
;;          :where [[?o :order/customer ?c]
;;                  [?o :order/items ?i]
;;                  [?i :item/amount ?a]
;;                  [?c :customer/id ?cid]
;;                  (not [?o :order/status :cancelled])]
;;          :in [?cid]}
;;  :args [100]}

(orders-total-query {:order-id 100})

;; {:query {:find [?c ?o (sum ?a)],
;;          :where [[?o :order/id ?id]
;;                  [?o :order/customer ?c]
;;                  [?o :order/items ?i]
;;                  [?i :item/amount ?a]
;;                  (not [?o :order/status :cancelled])],
;;          :in [?id]},
;;  :args [100]}

(orders-total-query {:order-id 100 :customer-id 100})
;; {:query {:find [?c ?o (sum ?a)],
;;          :where [[?o :order/id ?id]
;;                  [?o :order/customer ?c]
;;                  [?o :order/items ?i]
;;                  [?i :item/amount ?a]
;;                  [?c :customer/id ?cid]
;;                  (not [?o :order/status :cancelled])],
;;          :in [?id ?cid]},
;;  :args [100 100]}
```


## License

Copyright fmnoise © 2023
