
type t = {
  db: Caqti_lwt.connection;
}

let create url =
  let open PgMonad.Let_syntax in
  let%map db = Caqti_lwt.connect (Uri.of_string url) in
  { db; }

let transactions_query =
  Caqti_request.collect
    Caqti_type.(tup3 string int int) Caqti_type.(tup2 string string)
    "SELECT hash, trace FROM transactions t
    WHERE t.to = ? AND trace IS NOT NULL
    LIMIT ?
    OFFSET ?"

let vulnerable_contracts_query =
  Caqti_request.collect
    Caqti_type.string Caqti_type.string
    "SELECT address FROM contracts c
    JOIN vulnerability_reports vr on c.address = vr.contract_address
    WHERE vr.vulnerability_name = ?"

let get_vulnerable_contracts { db = (module Db: Caqti_lwt.CONNECTION); _ } vulnerability =
  Db.collect_list vulnerable_contracts_query vulnerability

let get_contract_transactions ?(limit=30) ?(offset=0)
    { db = (module Db: Caqti_lwt.CONNECTION); _ } address =
  Db.collect_list transactions_query (address, limit, offset)
