open Core

type t = {
  db: Caqti_lwt.connection;
}

let create url =
  let open PgMonad.Let_syntax in
  let%map db = Caqti_lwt.connect (Uri.of_string url) in
  { db; }

let transactions_query =
  Caqti_request.collect
    Caqti_type.(tup3 string int int) Caqti_type.(tup4 string string string int)
    "SELECT t.hash, t.trace, t.to, t.\"blockNumber\"
     FROM transactions t
     WHERE t.to = ?
        AND trace IS NOT NULL
     LIMIT ?
     OFFSET ?"

let indirect_transactions_query =
  Caqti_request.collect
    Caqti_type.(tup3 string int int) Caqti_type.(tup4 string string string int)
    "SELECT t.hash, t.trace, t.to, t.\"blockNumber\"
     FROM transactions t
     WHERE t.hash IN (
       SELECT t.hash
       FROM transactions t
       WHERE t.to = $1
         AND t.trace IS NOT NULL
       UNION
       SELECT t.hash
       FROM transactions t
              JOIN traces tr
                   ON tr.hash = t.hash
       WHERE (tr.to = $1 OR tr.from = $1)
         AND trace IS NOT NULL
     )
     LIMIT $2
     OFFSET $3"

let vulnerable_contracts_query =
  Caqti_request.collect
    Caqti_type.string Caqti_type.string
    "SELECT DISTINCT address FROM contracts c
    JOIN vulnerability_reports vr ON c.address = vr.contract_address
    WHERE vr.vulnerability_name = ?"

let report_queries =
  Caqti_request.collect
    Caqti_type.unit Caqti_type.(tup2 string string)
    "SELECT vr.contract_address, vr.vulnerability_name FROM vulnerability_reports vr
      JOIN vulnerabilities v ON vr.vulnerability_name = v.name
      JOIN tools t ON vr.tool_name = t.name
      WHERE t.checked AND v.checked"

let get_vulnerable_contracts { db = (module Db: Caqti_lwt.CONNECTION); _ } vulnerability =
  Db.collect_list vulnerable_contracts_query vulnerability

let get_contract_vulnerabilities { db = (module Db: Caqti_lwt.CONNECTION); _ } =
  let open PgMonad.Let_syntax in
  let%map reports = Db.collect_list report_queries () in
  let f acc (address, vuln) = Map.add_multi acc ~key:address ~data:vuln in
  List.fold reports ~init:String.Map.empty ~f

let get_contract_transactions ?(include_indirect=false) ?(limit=10) ?(offset=0)
    { db = (module Db: Caqti_lwt.CONNECTION); _ } address =
  let query = if include_indirect then indirect_transactions_query else transactions_query in
  Db.collect_list query (address, limit, offset)

let disconnect { db = (module Db: Caqti_lwt.CONNECTION); _ } =
  Lwt.map (fun v -> Result.Ok v) (Db.disconnect ())
