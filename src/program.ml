open Core

module Contract = struct
  type t = {
    name: String.t;
    bytecode: String.t;
    ops: Op.t List.t;
    sources: (String.t * String.t) List.t Option.t;
    sourcemap: Sourcemap.t Option.t;
  }

  let get_position offset ranges =
    let rec search left right =
      if left + 1 >= right then (left + 1, offset - ranges.(left))
      else
        let middle = (left + right + 1) / 2 in
        let value = ranges.(middle) in
        if offset = value then (middle + 1, offset - value)
        else if offset >= value then search middle right
        else search left (middle - 1)
    in
    search 0 (Array.length ranges)

  let format_sourcemap t =
    let sourcemap = (Option.value_exn ~message:"sourcemap not available" t.sourcemap) in

    let acc_range offset acc elem = if elem = '\n' then offset :: acc else acc in
    let make_ranges (_filename, source) = Array.of_list_rev (String.foldi ~init:[0] ~f:acc_range source) in
    let source_ranges = Option.map ~f:(List.map ~f:make_ranges) t.sources in

    let format_mapping mapping =
      let open Sourcemap.Mapping in
      let idx = mapping.source_index - 1 in
      if idx < 0 then "NA" else
        let stop = mapping.start + mapping.length in
        match source_ranges with
        | None -> Printf.sprintf "%d-%d" mapping.start stop
        | Some r when idx >= List.length r -> "NA"
        | Some all_ranges ->
          let prefix = if idx = 0 then "" else fst (List.nth_exn (Option.value_exn t.sources) idx) in
          let ranges = List.nth_exn all_ranges idx in
          let (start_line, start_col) = get_position mapping.start ranges in
          let (stop_line, stop_col) = get_position stop ranges in
          Printf.sprintf "%s%d:%d-%d:%d" prefix start_line start_col stop_line stop_col
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
    let string_opcodes = List.fold ~init:([], 1) ~f t.ops |> fst |> List.rev in
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

let format_ops ?contract:(contract_name=None) ?(show_pc=false) ?(show_sourcemap=false) t =
  let contract = match contract_name with
  | None -> List.hd_exn t.contracts
  | Some name -> List.find_exn ~f:(fun v -> v.Contract.name = name) t.contracts
  in
  Contract.format_ops ~show_pc ~show_sourcemap contract


let of_json ?(filename=None) json_string =
  let open Yojson.Safe.Util in
  let json = json_string |> Yojson.Safe.from_string in
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
    let f relative_path =
      let source_path = Filename.concat directory relative_path in
      match Sys.file_exists source_path with
      | `Yes -> Some (relative_path, (In_channel.read_all source_path))
      | `Unknown | `No -> None
    in
    Option.all (List.map ~f relative_paths)
  in
  let get_contract name =
    let open Contract in
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
