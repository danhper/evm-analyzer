open Evm_analyzer

let check_ops = Alcotest.(check (list Testable.opcode))

let simple () =
  let actual = OpcodeParser.parse_bytecode "515000" in
  let expected = [Op.Mload; Op.Pop; Op.Stop] in
  check_ops "parse simple string" expected actual

let with_0x_prefix () =
  let actual = OpcodeParser.parse_bytecode "0x30" in
  let expected = [Op.Address] in
  check_ops "supports 0x prefix" expected actual

let with_arguments () =
  let actual = OpcodeParser.parse_bytecode "600180" in
  let expected = [Op.Push (1, BigInt.one); Op.Dup 1] in
  check_ops "accepts arguments" expected actual

let with_missing_bytes () =
  let actual = OpcodeParser.parse_bytecode "6200" in
  let expected = [Op.Unknown "00"] in
  check_ops "should not fail with missing bytes" expected actual

let parser_tests = [
  "simple", `Quick, simple;
  "with_arguments", `Quick, with_arguments;
  "with_0x_prefix", `Quick, with_0x_prefix;
  "with_missing_bytes", `Quick, with_missing_bytes;
]
