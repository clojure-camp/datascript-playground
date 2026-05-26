
(require '[datascript.core :as d])

(def conn (atom (d/create-conn)))

(defn update-schema! [conn schema-updates]
  (let [current-db (d/db @conn)
        new-schema (merge (d/schema current-db) schema-updates)]
    (reset! conn
            (d/conn-from-db
             (d/init-db (d/datoms current-db :eavt) new-schema)))))

;; Initialize db and schema
(reset! conn
        (d/create-conn {:person/friend
                        {:db/cardinality :db.cardinality/many
                         :db/valueType :db.type/ref}
                        :person/email
                        {:db/unique :db.unique/identity}}))

;; Seed data
(d/transact! @conn
             [{:db/id -1
               :person/name "Alice"
               :person/email "alice@example.com"
               :person/age 30}
              {:db/id -2
               :person/name "Bob"
               :person/email "bob@example.com"
               :person/age 25}
              {:db/id -3
               :person/name "Charlie"
               :person/email "charlie@example.com"
               :person/age 35}
              {:db/id -1
               :person/friend -2}
              {:db/id -1
               :person/friend -3}])

;; Add another person
(d/transact! @conn
             [{:person/name "Edgar"
               :person/age 29
               :person/email "edgar@example.com"}])

;; Add via :db/add
(d/transact! @conn
             [[:db/add -1 :person/name "Frank"]
              [:db/add -1 :person/email "frank@example.com"]])

;; Update an entity
(d/transact! @conn
             [{:person/email "alice@example.com"
               :person/friend [:person/email "edgar@example.com"]}])

;; Retract attr
(d/transact! @conn
             [[:db/retract [:person/email "alice@example.com"]
               :person/friend [:person/email "edgar@example.com"]]])

;; Retract entity
(d/transact! @conn
             [[:db/retractEntity [:person/email "edgar@example.com"]]])

;; Names & ages
(d/q '[:find ?e ?name ?age
       :where
       [?e :person/name ?name]
       [?e :person/age ?age]]
     (d/db @conn))

;; Friends of Alice
(d/q '[:find [?name ...]
       :in $ ?email
       :where
       [?e :person/email ?email]
       [?e :person/friend ?f]
       [?f :person/name ?name]]
     (d/db @conn)
     "alice@example.com")

;; Pull *
(d/pull (d/db @conn) '[*] 1)

;; Query with pull q
(d/q '[:find [(pull ?e [:person/name
                        {:person/friend [:person/name]}]) ...]
       :where
       [?e :person/name _]]
     (d/db @conn))

;; Entity lookup
(:person/name (d/entity (d/db @conn) 1))

;; Entity lookup with ident
(:person/name (d/entity (d/db @conn) [:person/email "alice@example.com"]))

;; People younger than a given age
;; Uses a predicate function in the :where clause
(d/q '[:find ?name ?age
       :in $ ?max-age
       :where
       [?e :person/name ?name]
       [?e :person/age ?age]
       [(< ?age ?max-age)]]
     (d/db @conn)
     30)

;; Number of friends per person
;; Aggregate: count
(d/q '[:find ?name (count ?friend)
       :where
       [?e :person/name ?name]
       [?e :person/friend ?friend]]
     (d/db @conn))

;; Who has Bob as a friend
;; Uses reverse attribute lookup (:person/_friend) in pull
(d/pull (d/db @conn)
        '[:person/name {:person/_friend [:person/name]}]
        [:person/email "bob@example.com"])

;; Oldest person's age
;; Aggregate: max
(d/q '[:find (max ?age) .
       :where
       [_ :person/age ?age]]
     (d/db @conn))

;; Change schema
(update-schema! conn {:person/best-friend {:db/valueType :db.type/ref}})
