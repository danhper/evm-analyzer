open Core

let time_format = "%Y-%m-%d %H:%M:%S"

let now_str () =
  Time.format (Time.now ()) time_format ~zone:(force Time.Zone.local)

let reporter ppf =
  let report _src level ~over k msgf =
    let k _ = over (); k () in
    let with_stamp h _tags k ppf fmt =
      Format.kfprintf k ppf ("%a [%s] @[" ^^ fmt ^^ "@]@.")
        Logs.pp_header (level, h) (now_str ())
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }

let initialize ?(level=Logs.Info) () =
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some level)
