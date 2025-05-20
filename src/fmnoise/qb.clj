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

(defn or-join [q conditions & [values]]
  (cond-> q
    (seq conditions)
    (update-in [:query :where] (fnil conj []) (cons 'or-join conditions))

    (map? values)
    (as-> $ (reduce-kv (fn [acc k v] (in acc k v)) $ values))

    (and values (not (map? values)))
    (in (-> conditions flatten last) values)))

(defn exclude [q binding values & [input-name]]
  (let [excl-binding (or input-name (symbol (str (name binding) "-excluded")))]
    (-> q
        (where-not [(list excl-binding binding)])
        (add-input excl-binding (if (set? values) values (set values))))))

(defn where-not
  "Adds `not` condition to query. Accepts optional value.
  If value is set, it's used as function to filter out conditions"
  ([q condition]
   (cond-> q
     condition (where (list 'not condition))))
  ([q condition value]
   (let [binding (-> condition flatten last)]
     (if (set? value)
       (-> q
           (where condition)
           (exclude binding value))
       (-> q
           (where-not condition)
           (in binding value))))))

(defn where
  "Adds condition to query. Accepts optional value.
  If value is nil, condition is transformed into (not [...]).
  If value is `set?`, the corresponding collection binding is addded"
  ([q condition]
   (cond-> q
     condition (update-in [:query :where] (fnil conj []) condition)))
  ([q condition value]
   (if
     (nil? value)
     (where-not q condition)
     (let [binding (-> condition flatten last)
           regexp? (instance? java.util.regex.Pattern value)]
       (cond-> (where q condition)
         regexp? (where [(list 're-find value binding)])
         (not regexp?) (in binding value))))))

(defn where?
  "Adds condition to query only if supplied value is not nil.
  Accepts either query condition and value or map/list/vector of conditions and values eg:
  (where? q {'[?e :order/id ?id] id
             '[?e :order/customer ?c] customer}
  (where? q ['[?e :order/id ?id] id
             '[?e :order/customer ?c] customer]
  "
  ([q condition value]
   (cond-> q
     (some? value) (where condition value)))
  ([q cond-vals]
   (if (map? cond-vals)
     (reduce-kv (fn [acc condition value]
                  (where? acc condition value))
                q
                cond-vals)
     (->> cond-vals
          (partition 2)
          (reduce (fn [acc [condition value]]
                    (where? acc condition value))
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

(defn map->query
  "Transforms key-value map into query map. By default entity is bound as ?e but this can be redefined with `:find` meta supplied with map.
  Another supported meta attributes are `:as` for defining query keys and `:first` which will return scalar value

  (map->query {:user/id 1 :user/type :admin})
  ;; => {:query {:find [[?e ...]], :where [[?e :user/id ?user-id] [?e :user/type ?user-type]], :in [?user-id ?user-type]}, :args [1 :admin]}

  (map->query ^{:find '?user} {:user/id 1})
  ;; => {:query {:find [[?user ...]], :where [[?user :user/id ?user-id]], :in [?user-id]}, :args [1]}

  (map->query ^{:find '?user :as :user} {:user/id 1})
  ;; => {:query {:find [?user], :keys [:user], :where [[?user :user/id ?user-id]], :in [?user-id]}, :args [1]}"

  ([conditions] (map->query nil conditions))
  ([src conditions]
   (when (seq conditions)
     (when (> (count conditions) 8)
       (throw (IllegalArgumentException. "Query order is not preserved for conditions map with more than 8 keys")))
     (let [cmeta (meta conditions)
           key (:as cmeta)
           first? (:first cmeta)
           find-binding (or (:find cmeta) '?e)
           binding (if (symbol? find-binding)
                     find-binding
                     (-> find-binding flatten first))
           find-fn (cond
                     key find
                     first? find-scalar
                     :else find-coll)
           q (cond-> (find-fn (if key {key binding} find-binding))
               src (in src))]
       (reduce-kv (fn [acc k v]
                    (cond
                      (and (list? v) (= 'not (first v)))
                      (where-not acc [binding k (->binding k)] (last v))

                      (nil? v)
                      (where-not acc [binding k])

                      (= '_ v)
                      (where acc [binding k])

                      (symbol? v)
                      (where acc [binding k v])

                      :else
                      (where acc [binding k (->binding k)] v)))
                  q
                  conditions)))))

(defn vector->query
  "Transforms vector with attributes and values into query map. By default entity is bound as ?e but this can be redefined with `:find` meta supplied with vector
  Another supported meta attributes are `:as` for defining query keys and `:first` which will return scalar value.

  (vector->query [:user/id 1 :user/type :admin])
  ;; => {:query {:find [[?e ...]], :where [[?e :user/id ?user-id] [?e :user/type ?user-type]], :in [?user-id ?user-type]}, :args [1 :admin]}

  (vector->query ^{:find '?user} [:user/id 1])
  ;; => {:query {:find [[?user ...]], :where [[?user :user/id ?user-id]], :in [?user-id]}, :args [1]}

  (vector->query ^{:find '?user :as :user} [:user/id 1])
  ;; => {:query {:find [?user], :keys [:user], :where [[?user :user/id ?user-id]], :in [?user-id]}, :args [1]}"

  ([conditions] (vector->query nil conditions))
  ([src conditions]
   (when (seq conditions)
     (when-not (even? (count conditions))
       (throw (IllegalArgumentException. "Vector should have even number of values")))
     (let [cmeta (meta conditions)
           key (:as cmeta)
           first? (:first cmeta)
           find-binding (or (:find cmeta) '?e)
           binding (if (symbol? find-binding)
                     find-binding
                     (-> find-binding flatten first))
           find-fn (cond
                     key find
                     first? find-scalar
                     :else find-coll)
           q (cond-> (find-fn (if key {key binding} find-binding))
               src (in src))]
       (->> conditions
            (partition 2)
            (reduce (fn [acc [k v]]
                      (cond
                        (and (list? v) (= 'not (first v)))
                        (where-not acc [binding k (->binding k)] (last v))

                        (nil? v)
                        (where-not acc [binding k])

                        (= '_ v)
                        (where acc [binding k])

                        (symbol? v)
                        (where acc [binding k v])

                        :else
                        (where acc [binding k (->binding k)] v)))
                    q))))))

(defn data->query
  ([conditions] (data->query nil conditions))
  ([src conditions]
   (cond
     (map? conditions)
     (map->query src conditions)

     (vector? conditions)
     (vector->query src conditions)

     (list? conditions)
     (vector->query src (vec conditions))

     :else
     (throw (IllegalArgumentException. (str "Cannot turn " (type conditions) " into query"))))))
