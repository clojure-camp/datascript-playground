(ns app.examples)

(def sets
  [{:id :basics
    :label "Basics"
    :examples
    [{:label "Initialize schema"
      :code (pr-str '(reset! conn
                             (d/create-conn {:person/friend
                                             {:db/cardinality :db.cardinality/many
                                              :db/valueType :db.type/ref}})))}
     {:label "Seed data"
      :code (pr-str '(d/transact! @conn
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
                                    :person/friend -3}]))}
     {:label "Names & ages"
      :code (pr-str '(d/q '[:find ?e ?name ?age
                            :where
                            [?e :person/name ?name]
                            [?e :person/age ?age]]
                          (d/db @conn)))}
     {:label "Pull *"
      :code (pr-str '(d/pull (d/db @conn) '[*] 1))}
     {:label "All datoms"
      :code (pr-str '(seq (d/datoms (d/db @conn) :eavt)))}
     {:label "Entity lookup"
      :code (pr-str '(d/entity (d/db @conn) 1))}
     {:label "Friends of Alice"
      :code (pr-str '(d/q '[:find ?name
                            :where
                            [1 :person/friend ?f]
                            [?f :person/name ?name]]
                          (d/db @conn)))}
     {:label "Add person"
      :code (pr-str '(d/transact! @conn
                                  [{:person/name "Dave"
                                    :person/age 28}]))}
     {:label "Retract attr"
      :code (pr-str '(d/transact! @conn
                                  [[:db/retract 2 :person/age 25]]))}]}])
