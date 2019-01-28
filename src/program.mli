open Core

module Contract: sig
  type t = {
    name: String.t;
    bytecode: String.t;
    opcodes: Opcode.t List.t;
    source: String.t Option.t;
    sourcemap: Sourcemap.t Option.t;
  }

  val format_opcodes: ?show_pc:bool -> ?show_sourcemap:bool -> t -> String.t
end

type t = {
  contracts: Contract.t List.t;
  filename: String.t Option.t;
}

val format_opcodes: ?contract:String.t Option.t -> ?show_pc:bool -> ?show_sourcemap:bool -> t -> String.t

val of_json: ?filename:String.t Option.t -> String.t -> t
val of_bytecode: ?filename:String.t Option.t -> String.t -> t
val of_channel: ?filename:String.t Option.t -> In_channel.t -> t
val from_file: String.t -> t
