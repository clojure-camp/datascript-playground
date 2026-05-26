
(require '[datascript.core :as d])

(def conn (atom (d/create-conn)))

(defn update-schema! [conn schema-updates]
  (let [current-db (d/db @conn)
        new-schema (merge (d/schema current-db) schema-updates)]
    (reset! conn
            (d/conn-from-db
             (d/init-db (d/datoms current-db :eavt) new-schema)))))

;; Initialize schema
(reset! conn
        (d/create-conn {:person/friend
                        {:db/cardinality :db.cardinality/many
                         :db/valueType :db.type/ref}}))

;; Seed data
(d/transact! @conn
             [{:db/id -1
               :person/name "Alice"
               :person/age 30}
              {:db/id -2
               :person/name "Bob"
               :person/age 25}
              {:db/id -3
               :person/name "Charlie"
               :person/age 35}
              {:db/id -1
               :person/friend -2}
              {:db/id -1
               :person/friend -3}])

;; Names & ages
(d/q '[:find ?e ?name ?age
       :where
       [?e :person/name ?name]
       [?e :person/age ?age]]
     (d/db @conn))

;; Pull *
(d/pull (d/db @conn) '[*] 1)


;; Pull within q
(d/q '[:find [(pull ?e [:person/name
                        {:person/friend [:person/name]}]) ...]
       :where
       [?e :person/name _]]
     (d/db @conn))

;; All datoms
(seq (d/datoms (d/db @conn) :eavt))

;; Entity lookup
(d/entity (d/db @conn) 1)

;; Friends of Alice
(d/q '[:find ?name
       :where
       [1 :person/friend ?f]
       [?f :person/name ?name]]
     (d/db @conn))

;; Add person
(d/transact! @conn
             [{:person/name "Dave"
               :person/age 28}])

;; Retract attr
(d/transact! @conn
             [[:db/retract 2 :person/age 25]])

;; Change schema — add :person/email as a unique identity attribute
(update-schema! conn {:person/email {:db/unique :db.unique/identity}})
