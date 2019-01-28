open Core

module Contract = struct
  type t = {
    name: String.t;
    bytecode: String.t;
    opcodes: Opcode.t List.t;
    source: String.t Option.t;
    sourcemap: String.t Option.t;
  }
end

type t = {
  contracts: Contract.t List.t;
  filename: String.t Option.t;
}

let format_opcodes ?contract:(contract_name=None) ?(show_pc=false) t =
  let contract = match contract_name with
  | None -> List.hd_exn t.contracts
  | Some name -> List.find_exn ~f:(fun v -> v.Contract.name = name) t.contracts
  in

  let f (result, pc) opcode =
    let pc_string = if show_pc then Printf.sprintf "%d " pc else "" in
    (result ^ pc_string ^ Opcode.to_string opcode ^ "\n", pc + Opcode.size opcode)
  in
  List.fold ~init:("", 1) ~f contract.Contract.opcodes |> fst

let of_json ?(filename=None) json_string =
  let open Yojson.Safe.Util in
  let json = json_string |> Yojson.Safe.from_string in
  let contracts_json = member "contracts" json in
  let get_source ~idx filename =
    let relative_path = json |> member "sourceList" |> index idx |> to_string in
    let directory = Filename.dirname filename in
    let source_path = Filename.concat directory relative_path in
    match Sys.file_exists source_path with
    | `Yes -> Some (In_channel.read_all source_path)
    | `Unknown | `No -> None
  in
  let get_contract idx name =
    let open Contract in
    let contract_json = member name contracts_json in
    let bytecode = contract_json |> member "bin-runtime" |> to_string in
    { name;
      bytecode;
      opcodes = OpcodeParser.parse_bytecode bytecode;
      sourcemap = contract_json |> member "srcmap-runtime" |> to_string_option;
      source = Option.bind filename ~f:(get_source ~idx);
    }
  in
  { contracts = List.mapi ~f:get_contract (keys contracts_json); filename; }

let of_bytecode ?(filename=None) bytecode =
  let open Contract in
  let get_name name = name |> Filename.basename |> Filename.split_extension |> fst in
  let name = Option.value_map ~default:"NA" ~f:get_name filename in
  let contracts = [{
    name;
    bytecode;
    opcodes = OpcodeParser.parse_bytecode bytecode;
    source = None;
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
