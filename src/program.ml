open Core

module Contract = struct
  module Source = struct
    type t = {
      filename: String.t;
      body: String.t;
      index: Int.t;
    }
  end

  type t = {
    name: String.t;
    bytecode: String.t;
    ops: Op.t List.t;
    sources: Source.t List.t Option.t;
    sourcemap: Sourcemap.t Option.t;
  }

  let get_simple_name { name; _ } =
    match String.split ~on:':' name with
    | [x] -> x
    | list -> List.last_exn list

  let get_position offset ranges =
    let rec search left right =
      if left + 1 >= right then (left + 1, offset - ranges.(left))
      else
        let middle = (left + right + 1) / 2 in
        let value = ranges.(middle) in
        if offset = value then (middle + 1, offset - value)
        else if offset >= value then search middle right
        else search left middle
    in
    search 0 (Array.length ranges)

  let format_sourcemap t =
    let open Source in
    let sourcemap = (Option.value_exn ~message:"sourcemap not available" t.sourcemap) in
    let acc_range offset (in_string, escaped, acc) elem =
      if escaped then (in_string, false, acc)
      else match in_string, elem with
      | `None, '\n' -> (`None, false, offset :: acc)
      | `None, '"' -> (`Double, false, acc)
      | `Double, '"' -> (`None, false, acc)
      | `None, '\'' -> (`Single, false, acc)
      | `Single, '\'' -> (`None, false, acc)
      | _, '\\' -> (in_string, true, acc)
      | _ -> (in_string, false, acc)
    in
    let make_ranges { body; _ } =
      let (_, _, ranges) = String.foldi ~init:(`None, false, [0]) ~f:acc_range body in
      Array.of_list_rev ranges
    in
    let source_ranges = Option.map ~f:(List.map ~f:make_ranges) t.sources in

    let format_mapping mapping =
      let open Sourcemap.Mapping in
      let idx = mapping.source_index in
      if idx < 0 then "NA" else
        let stop = mapping.start + mapping.length in
        match source_ranges with
        | None -> Printf.sprintf "%d-%d" mapping.start stop
        | Some all_ranges ->
          let sources = Option.value_exn t.sources in
          let f _index source = source.index = idx in
          begin match List.findi sources ~f with
          | None -> "NA"
          | Some (index, source) ->
            let prefix = if List.length sources = 1 then "" else source.filename ^ ":" in
            let ranges = List.nth_exn all_ranges index in
            let (start_line, start_col) = get_position mapping.start ranges in
            let (stop_line, stop_col) = get_position stop ranges in
            Printf.sprintf "%s%d:%d-%d:%d" prefix start_line start_col stop_line stop_col
          end
    in
    List.map ~f:format_mapping sourcemap

  let append_sourcemaps t ops =
    let formatted_sourcemap = format_sourcemap t in
    let length_diff = List.length ops - List.length formatted_sourcemap in
    let final_sourcemap = formatted_sourcemap @ List.init length_diff ~f:(Fn.const "NA") in
    let f (opcode, mapping) = opcode ^ " " ^ mapping in
    List.map ~f (List.zip_exn ops final_sourcemap)

  let format_ops ?(show_pc=false) ?(show_sourcemap=false) t =
    let f (result, pc) op =
      let pc_string = if show_pc then Printf.sprintf "%d " pc else "" in
      ((pc_string ^ Op.to_string op) :: result, pc + Op.size op)
    in
    let string_opcodes = List.fold ~init:([], 0) ~f t.ops |> fst |> List.rev in
    let formatted_opcodes =
      if show_sourcemap
        then append_sourcemaps t string_opcodes
        else string_opcodes
    in
    String.concat ~sep:"\n" formatted_opcodes ^ "\n"
end

type t = {
  contracts: Contract.t List.t;
  filename: String.t Option.t;
}

let format_ops ?(contract_name=None) ?(show_pc=false) ?(show_sourcemap=false) t =
  let available_contracts = String.concat ~sep:", " (List.map ~f:(fun c -> Contract.get_simple_name c) t.contracts) in
  let contract = match contract_name with
  | None -> List.hd t.contracts
  | Some name -> List.find ~f:(fun c -> Contract.get_simple_name c = name) t.contracts
  in
  match contract with
  | None ->
    failwithf "contract not found, available: %s" available_contracts ()
  | Some contract ->
    Contract.format_ops ~show_pc ~show_sourcemap contract

let json_version json =
  let open Yojson.Safe.Util in
  match member "schemaVersion" json with
  | `Null -> 1
  | `String semver -> begin match String.split semver ~on:'.' with
    | [major; _minor; _patch] -> Int.of_string major
    | _ -> failwithf "invalid semver: %s" semver ()
    end
  | v -> failwithf "unknown value for schemaVersion: %s" (to_string v) ()


let of_json_version_2 ?(filename=None) json =
  let open Yojson.Safe.Util in
  let open Contract in
  let bytecode = json |> member "deployedBytecode" |> to_string in
  let sourcemap = json |> member "deployedSourceMap" |> to_string |> Sourcemap.of_string in
  let source = json |> member "source" |> to_string in
  let source_path = json |> member "sourcePath" |> to_string in
  let ast_sourcemap = json |> member "ast" |> member "src" |> to_string |> Sourcemap.Mapping.of_string in
  let contract =
  { name = json |> member "contractName" |> to_string;
    bytecode;
    ops = OpcodeParser.parse_bytecode bytecode;
    sourcemap = Some sourcemap;
    sources = Some [{
      Source.filename = source_path;
      Source.body = source;
      Source.index = ast_sourcemap.Sourcemap.Mapping.source_index;
    }];
  }
  in
  { contracts = [contract]; filename; }


let of_json_version_1 ?(filename=None) json =
  let open Contract in
  let open Yojson.Safe.Util in
  let contracts_json = member "contracts" json in
  let source_list = match member "sourceList" json with
  | `List source_list -> Some (List.map ~f:to_string source_list)
  | _ -> None
  in
  let get_sources contract_json filename =
    let relative_paths =
      match member "metadata" contract_json, source_list with
      | `String raw_metadata, _ ->
        raw_metadata |> Yojson.Safe.from_string |> member "sources" |> keys
      | _, Some source_list -> source_list
      | _ -> failwith "metadata and sourceList not available"
    in
    let directory = Filename.dirname filename in
    let f index relative_path =
      let source_path = Filename.concat directory relative_path in
      match Sys.file_exists source_path with
      | `Yes -> Some { Source.filename = relative_path;
                       Source.body = In_channel.read_all source_path;
                       Source.index= index; }
      | `Unknown | `No -> None
    in
    Option.all (List.mapi ~f relative_paths)
  in
  let get_contract name =
    let contract_json = member name contracts_json in
    let bytecode = contract_json |> member "bin-runtime" |> to_string in
    let sourcemap = contract_json |> member "srcmap-runtime" |> to_string_option
                    |> Option.map ~f:Sourcemap.of_string in
    { name;
      bytecode;
      ops = OpcodeParser.parse_bytecode bytecode;
      sourcemap;
      sources = Option.bind filename ~f:(get_sources contract_json);
    }
  in
  { contracts = List.map ~f:get_contract (keys contracts_json); filename; }


let of_json ?(filename=None) json_string =
  let json = json_string |> Yojson.Safe.from_string in
  match json_version json with
  | 1 -> of_json_version_1 ~filename json
  | 2 -> of_json_version_2 ~filename json
  | n -> failwithf "unsupported schema version %d" n ()

let of_bytecode ?(filename=None) bytecode =
  let open Contract in
  let get_name name = name |> Filename.basename |> Filename.split_extension |> fst in
  let name = Option.value_map ~default:"NA" ~f:get_name filename in
  let contracts = [{
    name;
    bytecode;
    ops = OpcodeParser.parse_bytecode bytecode;
    sources = None;
    sourcemap = None;
  }]
  in
  { contracts; filename; }

let of_channel ?(filename=None) channel =
  let content = In_channel.input_all channel in
  if Option.value_map ~f:(String.is_suffix ~suffix:".json") ~default:false filename
    then of_json ~filename content
    else of_bytecode ~filename content

let from_file filename =
  let f = of_channel ~filename:(Some filename) in
  In_channel.with_file filename ~f
