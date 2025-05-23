(ns fmnoise.qb
  (:refer-clojure :exclude [find]))

(declare where where-not)

(defn add-input
  "Adds query input. Last optional agrument turns automatic expanding of collection source into collection binding"
  [q binding source & [expand-coll?]]
  (let [binding (if (and (not (vector? binding))
                         (coll? source)
                         expand-coll?)
                  [binding '...]
                  binding)]
    (-> q
        (update-in [:query :in] (fnil conj []) binding)
        (update :args (fnil conj []) source))))

(defn in
  "Adds query input. If binding for input is not specified, its added as `$` as first argument.
  If source is `set?`, the corresponding collection binding is added"
  ([q src]
   (-> q
       (update-in [:query :in] (partial into ['$]))
       (update :args (partial into [src]))))
  ([q binding source & inputs]
   (when (some-> inputs count odd?)
     (throw (IllegalArgumentException. "in requires an even number of inputs")))
   (if inputs
     (->> inputs
          (into [binding source])
          (partition 2)
          (reduce (fn [acc [binding source]]
                    (if source (in acc binding source) acc)) q))
     (add-input q binding source (set? source)))))

(defn find
  "Adds bindings to query `:find`"
  ([bindings] (find nil bindings))
  ([q bindings & [override?]]
   (let [[bs ks] (if (map? bindings)
                   [(vec (vals bindings)) (vec (keys bindings))]
                   [bindings])
         bs (if (not (vector? bs)) [bs] bs)]
     (cond-> q
       override? (update :query #(dissoc % :find :keys))
       bs (update-in [:query :find] #(-> % vec (into bs)))
       ks (update-in [:query :keys] #(-> % vec (into ks)))))))

(defn find-coll
  "Adds collection binding to query `:find`"
  ([binding] (find-coll nil binding))
  ([q binding]
   (assoc-in q [:query :find] (if (vector? binding) binding [[binding '...]]))))

(defn find-scalar
  "Adds scalar binding to query `:find`"
  ([binding] (find-scalar nil binding))
  ([q binding] (assoc-in q [:query :find] (if (vector? binding) binding [binding '.]))))

(defn with
  "Adds `with` part to query"
  [q & bindings]
  (update-in q [:query :with] (fnil into []) bindings))

(defn- join [q join-type conditions inputs]
  (cond-> q
    (seq conditions)
    (update-in [:query :where] (fnil conj []) (cons join-type conditions))

    (map? inputs)
    (as-> $ (reduce-kv (fn [acc k v] (in acc k v)) $ inputs))

    (and inputs (not (map? inputs)))
    (in (-> conditions flatten last) inputs)))

(defn or-join [q conditions & [inputs]]
  (join q 'or-join conditions inputs))

(defn not-join [q conditions & [inputs]]
  (join q 'not-join conditions inputs))

(defn exclude [q binding input & [input-name]]
  (let [exclusion (or input-name (symbol (str (name binding) "-input")))]
    (-> q
        (where-not [(list exclusion binding)])
        (add-input exclusion (set input)))))

(defn where-not
  "Adds `not` condition to query. Accepts optional value.
  If value is set, it's used as function to filter out conditions"
  ([q condition]
   (cond-> q
     condition (where (list 'not condition))))
  ([q condition input]
   (let [binding (-> condition flatten last)]
     (if (set? input)
       (-> q
           (where condition)
           (exclude binding input))
       (-> q
           (where-not condition)
           (in binding input))))))

(defn where-missing
  "Adds `missing?` condition to query. Condition may have or may not have source binding, $ is used then"
  [q condition]
  (cond-> q
    condition (where [(concat (list 'missing?)
                              (if (> 3 (count condition))
                                (into '[$] condition)
                                condition))])))

(defn where
  "Adds condition to query. Accepts optional input
  If input is nil, condition is transformed into (not [...]).
  If input is `set?`, the corresponding collection binding is addded"
  ([q condition]
   (cond-> q
     condition (update-in [:query :where] (fnil conj []) condition)))
  ([q condition input]
   (if (nil? input)
     (where-not q condition)
     (let [binding (-> condition flatten last)
           regexp? (instance? java.util.regex.Pattern input)]
       (cond-> (where q condition)
         regexp? (where [(list 're-find input binding)])
         (not regexp?) (in binding input))))))

(defn where?
  "Adds condition to query only if supplied input is not nil.
  Accepts either query condition and value or map/list/vector of conditions and values eg:
  (where? q {'[?e :order/id ?id] id
             '[?e :order/customer ?c] customer}
  (where? q ['[?e :order/id ?id] id
             '[?e :order/customer ?c] customer]
  "
  ([q condition input]
   (cond-> q
     (some? input) (where condition input)))
  ([q cond-vals]
   (if (map? cond-vals)
     (reduce-kv (fn [acc condition input]
                  (where? acc condition input))
                q
                cond-vals)
     (->> cond-vals
          (partition 2)
          (reduce (fn [acc [condition input]]
                    (where? acc condition input))
                  q)))))

(defn where*
  "Adds multiple conditions/values from supplied collection to query using `where`.
  If condition needs to have supplied value, it should be wrapped in vector eg
  (where* q
    [[?e :order/id]
     [['?e :order/customer '?c] customer]"
  [q conditions]
  (reduce (fn [acc condition]
            (if (vector? (first condition))
              (case (count condition)
                1 (where acc (first condition))
                2 (where acc (first condition) (last condition))
                (throw (IllegalArgumentException. (str "Cannot parse query element: " condition))))
              (where acc condition)))
          q
          conditions))

(defn where-not*
  "Adds multiple conditions/values from supplied collection to query using `where-not`.
  If condition needs to have supplied value, it should be wrapped in vector eg
  (where-not* q
    [[?e :order/cancelled]
     ['?e :order/status '?s] status])"
  [q conditions]
  (reduce (fn [acc condition]
            (if (vector? (first condition))
              (let [[c v] condition]
                (if v
                  (where-not acc c v)
                  (where-not acc c)))
              (where-not acc condition)))
          q
          conditions))

(defn where->
  "Adds multiple conditions/values to query using `where`.
  If condition needs to have supplied value, it should be wrapped in vector eg
  (where-> q
    ['?e :order/id]
    [['?e :order/customer '?c] customer]"
  [q & conditions]
  (where* q conditions))

(defn ->binding [v & [uniq?]]
  (let [name (if (instance? clojure.lang.Named v)
               (str "?" (when-let [n (namespace v)] (str n "-")) (name v))
               (str "?" v))]
    (symbol (if uniq? (gensym (str name "_")) name))))

(defn- process-condition [acc k v]
  (let [{::keys [from in] :as options} (meta acc)
        nil-mode (::nil options)]
    (cond
      (symbol? k)
      (where acc v)

      (and (coll? v) (symbol? (first v)))
      (let [[func value] v
            binding (->binding k)
            input-binding (symbol (str (name binding) "-input"))]
        (cond-> (where acc [from k binding])
          value (where [(list func binding input-binding)] value)
          (not value) (where [(list func binding)])))

      (nil? v)
      (case nil-mode
        :not (where-not acc [from k])
        :skip acc
        :missing (where-missing acc [in from k])
        (throw (IllegalArgumentException. (str nil-mode " is not valid nil handling option, only :not, :skip and :missing are allowed"))))

      (= '_ v)
      (where acc [from k])

      (symbol? v)
      (where acc [from k v])

      :else
      (where acc [from k (->binding k)] v))))

(defn- map->query
  [query conditions]
  (when (seq conditions)
    (when (> (count conditions) 8)
      (throw (IllegalArgumentException. "Query order is not preserved for conditions map with more than 8 keys")))
    (reduce-kv process-condition query conditions)))

(defn- vector->query
  [query conditions]
  (when (seq conditions)
    (when-not (even? (count conditions))
      (throw (IllegalArgumentException. "Vector should have even number of values")))
    (->> conditions
         (partition 2)
         (reduce (fn [acc [k v]] (process-condition acc k v)) query))))

(defn query
  "Transforms map or vector into query map.
  Query options can be supplied as keys (only for maps):
  `:in` for source binding (defaults to $)
  `:find` for defining result binding (defaults to ?e)
  `:from` for defining entity binding (defaults to ?e)
  `:nil` which indicates how to handle nils, defaults to `:missing`, possible options are:
     `:missing` - replace nil values with [(missing? ...)]
     `:not` - replace nil values with [(not [...])]
     `:skip` - skip nil values
  `:aggregate` which can contain either a keyword (for attribute) or symbol (for function) or collection of function symbol and keyword in any order like [:order/total 'sum] or ['sum :order/total]
  when aggregation is used, `:with` instruction is automatically added
  `:where` map or vector of query conditions

  (query {:user/id 1 :user/type :admin})
  ;; => {:query {:find [[?e ...]], :where [[?e :user/id ?user-id] [?e :user/type ?user-type]], :in [?user-id ?user-type]}, :args [1 :admin]}

  (query {:find '?user :where {:user/id 1}})
  ;; => {:query {:find [[?user ...]], :where [[?user :user/id ?user-id]], :in [?user-id]}, :args [1]}

  (query {:aggregate ['sum :order/total] :where {:order/id '_}})
  ;; => {:query {:find [(sum ?order-total) .], :where [[?e :order/id] [?e :order/total ?order-total]]}}

  (query {:aggregate :order/customer :where [:order/id '_]})
  ;; => {:query {:find [[?order-customer ...]], :where [[?e :order/id] [?e :order/customer ?order-customer]]}}

  (query [:user/id 1 :user/type :admin])
  ;; => {:query {:find [[?e ...]], :where [[?e :user/id ?user-id] [?e :user/type ?user-type]], :in [?user-id ?user-type]}, :args [1 :admin]}
"
  ([data] (query nil data))
  ([src data]
   {:pre [(or (map? data) (vector? data))]}
   (let [{:keys [aggregate] :as options} (when (map? data) data)
         src-binding (or (:in options) '$)
         find-binding (or (:find options) (:from options) '?e)
         from (let [binding (or (:from options) find-binding)]
                (if (coll? binding)
                  (->> binding flatten first)
                  binding))
         aggr-func (cond
                     (coll? aggregate) (->> aggregate (filter symbol?) first)
                     (symbol? aggregate) aggregate)
         aggr-attr (cond
                     (coll? aggregate) (->> aggregate (filter keyword?) first)
                     (keyword? aggregate) aggregate)
         aggr-binding (->binding aggr-attr)
         query (cond-> (find-coll find-binding)
                 src (in src)
                 true (with-meta {::find find-binding ::from from ::in src-binding ::nil (or (:nil options) :missing)}))
         conditions (:where data data)
         query (if (map? conditions)
                 (map->query query conditions)
                 (vector->query query conditions))]
     (cond-> query
       aggr-attr (-> (where [from aggr-attr aggr-binding]) (with from))
       (and aggr-attr aggr-func) (find-scalar (list aggr-func aggr-binding))
       (and aggr-func (not aggr-attr)) (find-scalar (list aggr-func from))
       (and aggr-attr (not aggr-func)) (find-coll aggr-binding)))))
