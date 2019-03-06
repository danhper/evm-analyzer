open Evm_analyzer

let assert_equal ~message actual expected =
  Alcotest.(check (list Testable.opcode)) message actual expected

let simple () =
  let actual = OpcodeParser.parse_bytecode "515000" in
  let expected = [Op.Mload; Op.Pop; Op.Stop] in
  assert_equal ~message:"parse simple string" actual expected

let with_0x_prefix () =
  let actual = OpcodeParser.parse_bytecode "0x30" in
  let expected = [Op.Address] in
  assert_equal ~message:"supports 0x prefix" actual expected

let with_arguments () =
  let actual = OpcodeParser.parse_bytecode "600180" in
  let expected = [Op.Push (1, BigInt.one); Op.Dup 1] in
  assert_equal ~message:"accepts arguments" actual expected

let parser_tests = [
  "simple", `Quick, simple;
  "with_arguments", `Quick, with_arguments;
  "with_0x_prefix", `Quick, with_0x_prefix;
]
