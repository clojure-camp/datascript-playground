
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
        (d/create-conn {:user/follows
                        {:db/cardinality :db.cardinality/many
                         :db/valueType :db.type/ref}
                        :user/email
                        {:db/unique :db.unique/identity}}))

;; Seed data
(d/transact! @conn
             [{:db/id -1
               :user/name "Alice"
               :user/email "alice@example.com"
               :user/join-year 2019}
              {:db/id -2
               :user/name "Bob"
               :user/email "bob@example.com"
               :user/join-year 2021}
              {:db/id -3
               :user/name "Charlie"
               :user/email "charlie@example.com"
               :user/join-year 2018}
              {:db/id -1
               :user/follows -2}
              {:db/id -1
               :user/follows -3}])

;; Add another person
(d/transact! @conn
             [{:user/name "Edgar"
               :user/email "edgar@example.com"
               :user/join-year 2022}])

;; Add via :db/add
(d/transact! @conn
             [[:db/add -1 :user/name "Frank"]
              [:db/add -1 :user/email "frank@example.com"]])

;; Update an entity
(d/transact! @conn
             [{:user/email "alice@example.com"
               :user/follows [:user/email "edgar@example.com"]}])

;; Retract attr
(d/transact! @conn
             [[:db/retract [:user/email "alice@example.com"]
               :user/follows [:user/email "edgar@example.com"]]])

;; Retract entity
(d/transact! @conn
             [[:db/retractEntity [:user/email "edgar@example.com"]]])

;; Names & join years
(d/q '[:find ?e ?name ?year
       :where
       [?e :user/name ?name]
       [?e :user/join-year ?year]]
     (d/db @conn))

;; Who does Alice follow
(d/q '[:find [?name ...]
       :in $ ?email
       :where
       [?e :user/email ?email]
       [?e :user/follows ?f]
       [?f :user/name ?name]]
     (d/db @conn)
     "alice@example.com")

;; Pull *
(d/pull (d/db @conn) '[*] 1)

;; Query with pull q
(d/q '[:find [(pull ?e [:user/name
                        {:user/follows [:user/name]}]) ...]
       :where
       [?e :user/name _]]
     (d/db @conn))

;; Entity lookup
(:user/name (d/entity (d/db @conn) 1))

;; Entity lookup with ident
(:user/name (d/entity (d/db @conn) [:user/email "alice@example.com"]))

;; Users who joined before a given year
;; Uses a predicate function in the :where clause
(d/q '[:find ?name ?year
       :in $ ?before
       :where
       [?e :user/name ?name]
       [?e :user/join-year ?year]
       [(< ?year ?before)]]
     (d/db @conn)
     2021)

;; Number of accounts each user follows
;; Aggregate: count
(d/q '[:find ?name (count ?followed)
       :where
       [?e :user/name ?name]
       [?e :user/follows ?followed]]
     (d/db @conn))

;; Who follows Bob
;; Uses reverse attribute lookup (:user/_follows) in pull
(d/pull (d/db @conn)
        '[:user/name {:user/_follows [:user/name]}]
        [:user/email "bob@example.com"])

;; Earliest join year
;; Aggregate: min
(d/q '[:find (min ?year) .
       :where
       [_ :user/join-year ?year]]
     (d/db @conn))

;; Change schema
(update-schema! conn {:post/author {:db/valueType :db.type/ref}})

;; Add posts
(d/transact! @conn
             [{:post/content "Hello"
               :post/author [:user/email "alice@example.com"]}
              {:post/content "World"
               :post/author [:user/email "alice@example.com"]}])
