open Core

module Tags: sig
  type t = Yojson.Safe.json String.Table.t
  val to_string: t -> String.t
  val pp: Format.formatter -> Yojson.Safe.json String.Table.t -> unit
end

module StackValue: sig
  type t = {
    value: BigInt.t;
    tags: Tags.t;
  } [@@deriving show]

  val create: BigInt.t -> t

  val set_tag: t -> key:String.t -> value:Yojson.Safe.json -> unit
  val get_tag: t -> String.t -> Yojson.Safe.json Option.t
  val get_tag_exn: t -> String.t -> Yojson.Safe.json
end

module FullTrace: sig
  type t = {
    trace: Trace.t;
    args: StackValue.t List.t;
    result: StackValue.t Option.t;
  }
end

module Tagger: sig
  type t = FullTrace.t -> unit
end
