let () =
  Alcotest.run "Evm_analyzer" [
    ("opcode_parser", TestOpcodeParser.parser_tests)
  ]
