open Core

module Contract: sig
  module Source: sig
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

  val format_ops: ?show_pc:bool -> ?show_sourcemap:bool -> t -> String.t
end

type t = {
  contracts: Contract.t List.t;
  filename: String.t Option.t;
}

val format_ops: ?contract:String.t Option.t -> ?show_pc:bool -> ?show_sourcemap:bool -> t -> String.t

val of_json: ?filename:String.t Option.t -> String.t -> t
val of_bytecode: ?filename:String.t Option.t -> String.t -> t
val of_channel: ?filename:String.t Option.t -> In_channel.t -> t
val from_file: String.t -> t
